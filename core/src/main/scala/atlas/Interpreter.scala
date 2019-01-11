package atlas

import atlas.syntax._

import cats._
import cats.data._
import cats.implicits._

import cats.mtl._
import cats.mtl.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

object Interpreter {
  implicit def apply[F[_]](implicit monadError: MonadError[F, RuntimeError], monadState: MonadState[F, Env]): Interpreter[F] =
    new Interpreter[F]

  def evalAs[F[_], A](expr: Expr, env: Env = Env.create)(implicit monad: MonadError[F, RuntimeError], interpreter: Interpreter[StateT[F, Env, ?]], dec: ValueDecoder[A]): F[A] = {
    import interpreter._
    evalExpr(expr).flatMap(value => liftEither(value.toScala)).runA(env)
  }

  def eval[F[_]](expr: Expr, env: Env = Env.create)(implicit monad: MonadError[F, RuntimeError], interpreter: Interpreter[StateT[F, Env, ?]]): F[Value] = {
    import interpreter._
    evalExpr(expr).runA(env)
  }

  val sync: Interpreter[Lambda[B => StateT[Lambda[A => EitherT[Eval, RuntimeError, A]], Env, B]]] =
    new Interpreter

  def async(implicit ec: ExecutionContext): Interpreter[Lambda[B => StateT[Lambda[A => EitherT[Future, RuntimeError, A]], Env, B]]] =
    new Interpreter
}

class Interpreter[F[_]](
  implicit
  val monadError: MonadError[F, RuntimeError],
  val monadState: MonadState[F, Env]
) extends InfixImpl with PrefixImpl {
  def evalExpr(expr: Expr): F[Value] =
    expr match {
      case expr: RefExpr     => evalRef(expr)
      case expr: AppExpr     => evalApp(expr)
      case expr: InfixExpr   => evalInfix(expr)
      case expr: PrefixExpr  => evalPrefix(expr)
      case expr: FuncExpr    => evalFunc(expr)
      case expr: BlockExpr   => evalBlock(expr)
      case expr: SelectExpr  => evalSelect(expr)
      case expr: CondExpr    => evalCond(expr)
      case CastExpr(expr, _) => evalExpr(expr)
      case ParenExpr(expr)   => evalExpr(expr)
      case expr: ObjExpr     => evalObj(expr)
      case expr: ArrExpr     => evalArr(expr)
      case StrExpr(value)    => pure(StrVal(value))
      case IntExpr(value)    => pure(IntVal(value))
      case DblExpr(value)    => pure(DblVal(value))
      case BoolExpr(value)   => pure(BoolVal(value))
      case NullExpr          => pure(NullVal)
    }

  def evalRef(ref: RefExpr): F[Value] =
    getVariable(ref.id)

  def evalApp(apply: AppExpr): F[Value] =
    for {
      func <- evalExpr(apply.func)
      args <- apply.args.traverse(evalExpr)
      ans  <- evalApp(func, args)
    } yield ans

  def evalApp(func: Value, args: List[Value]): F[Value] =
    func match {
      case closure : Closure => applyClosure(closure, args)
      case native  : Native  => applyNative(native, args)
      case value             => fail(s"Cannot call $value")
    }

  def evalInfix(infix: InfixExpr): F[Value] =
    for {
      arg1 <- evalExpr(infix.arg1)
      arg2 <- evalExpr(infix.arg2)
      ans  <- applyNative(infixImpl(infix.op), List(arg1, arg2))
    } yield ans

  def evalPrefix(prefix: PrefixExpr): F[Value] =
    for {
      arg  <- evalExpr(prefix.arg)
      ans  <- applyNative(prefixImpl(prefix.op), List(arg))
    } yield ans

  def evalFunc(func: FuncExpr): F[Value] =
    createClosure(func)

  def evalBlock(block: BlockExpr): F[Value] =
    pushScope {
      for {
        _    <- evalStmts(block.stmts)
        ans  <- evalExpr(block.expr)
      } yield ans
    }

  def evalStmts(stmts: List[Stmt]): F[Unit] =
    stmts.foldLeft(pure(())) { (a, b) =>
      a.flatMap(_ => evalStmt(b))
    }

  def evalStmt(stmt: Stmt): F[Unit] =
    stmt match {
      case stmt: ExprStmt    => evalExprStmt(stmt)
      case stmt: LetStmt     => evalLetStmt(stmt)
      case stmt: LetTypeStmt => pure(())
    }

  def evalLetStmt(stmt: LetStmt): F[Unit] =
    for {
      value <- evalExpr(stmt.expr)
      _     <- setVariable(stmt.varName, value)
    } yield ()

  def evalExprStmt(stmt: ExprStmt): F[Unit] =
    evalExpr(stmt.expr).map(_ => ())

  def evalSelect(select: SelectExpr): F[Value] =
    for {
      value  <- evalExpr(select)
      result <- evalSelect(value, select.field)
    } yield result

  def evalSelect(value: Value, id: String): F[Value] =
    value match {
      case ObjVal(fields) =>
        fields.collectFirst { case (n, v) if n == id => v } match {
          case Some(value) => value.pure[F]
          case None        => RuntimeError(s"Field not found: $id").raiseError[F, Value]
        }

      case other =>
        fail(s"Could not select field '$id' from $other")
    }

  def evalCond(cond: CondExpr): F[Value] =
    for {
      test   <- evalExpr(cond.test)
      result <- test match {
                  case BoolVal(true)  => evalExpr(cond.trueArm)
                  case BoolVal(false) => evalExpr(cond.falseArm)
                  case _              => fail(s"Conditional test was not a Boolean")
                }
    } yield result

  def evalObj(obj: ObjExpr): F[Value] =
    obj.fields.traverse { case (n, e) => evalExpr(e).map(v => (n, v)) }.map(ObjVal)

  def evalArr(arr: ArrExpr): F[Value] =
    arr.exprs.traverse(evalExpr).map(ArrVal)

  def applyClosure(closure: Closure, args: List[Value]): F[Value] =
    swapEnv(closure.env) {
      pushScope {
        for {
          env <- setVariables(closure.func.args.map(_.argName).zip(args))
          ans <- evalExpr(closure.func.body)
        } yield ans
      }
    }

  def applyNative(native: Native, args: List[Value]): F[Value] =
    native.run(args)(this)

  // Environment helpers ------------------------

  def getVariable(name: String): F[Value] =
    for {
      env   <- currentEnv
      value <- env.get(name) match {
                case Some(value) => pure(value)
                case None        => fail(s"Not in scope: $name")
              }
    } yield value

  def setVariable(name: String, value: Value): F[Unit] =
    inspectEnv(env => env.destructiveSet(name, value))

  def setVariables(bindings: Seq[(String, Value)]): F[Unit] =
    inspectEnv(env => env.destructiveSetAll(bindings))

  def createClosure(func: FuncExpr): F[Value] =
    inspectEnv(env => (Closure(func, env) : Value))

  def swapEnv[A](env: Env)(body: F[A]): F[A] =
    for {
      env0 <- currentEnv
      _    <- modifyEnv(_ => env)
      ans  <- body
      _    <- modifyEnv(_ => env0)
    } yield ans

  def pushScope[A](body: F[A]): F[A] =
    for {
      _   <- modifyEnv(_.push)
      ans <- body
      _   <- modifyEnv(_.pop)
    } yield ans

  def currentEnv: F[Env] =
    monadState.get

  def inspectEnv[A](func: Env => A): F[A] =
    monadState.inspect(func)

  def modifyEnv(func: Env => Env): F[Unit] =
    monadState.modify(func)

  // Error handling helpers ---------------------

  def pure[A](value: A): F[A] =
    value.pure[F]

  def fail[A](error: RuntimeError): F[A] =
    error.raiseError[F, A]

  def fail[A](msg: String, cause: Option[Throwable] = None): F[A] =
    fail(RuntimeError(msg, cause))

  def liftEither[A](either: Either[RuntimeError, A]): F[A] =
    either.fold(_.raiseError[F, A], _.pure[F])

  def catchNonFatal[A](body: => A): F[A] =
    try pure(body) catch { case NonFatal(exn) => fail("Error executing native code", Some(exn)) }
}
