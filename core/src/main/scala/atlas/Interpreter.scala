package atlas

import atlas.syntax._
import cats._
import cats.data._
import cats.implicits._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

object Interpreter {
  implicit def apply[F[_]](implicit monadError: MonadError[F, RuntimeError]): Interpreter[F] =
    new Interpreter[F]

  def evalAs[F[_], A](expr: Expr, env: Env = Env.create)(implicit interpreter: Interpreter[F], dec: ValueDecoder[A]): F[A] = {
    import interpreter._
    evalExpr(expr)(env.push).flatMap(value => liftEither(value.toScala))
  }

  def eval[F[_]](expr: Expr, env: Env = Env.create)(implicit interpreter: Interpreter[F]): F[Value] = {
    import interpreter._
    evalExpr(expr)(env.push)
  }

  val sync: Interpreter[EitherT[Eval, RuntimeError, ?]] =
    new Interpreter

  def async(implicit ec: ExecutionContext): Interpreter[EitherT[Future, RuntimeError, ?]] =
    new Interpreter
}

class Interpreter[F[_]](implicit val monadError: MonadError[F, RuntimeError]) extends InfixImpl with PrefixImpl {
  def evalExpr(expr: Expr)(implicit env: Env): F[Value] =
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

  def evalRef(ref: RefExpr)(implicit env: Env): F[Value] =
    getVariable(ref.id)

  def evalApp(apply: AppExpr)(implicit env: Env): F[Value] =
    for {
      func <- evalExpr(apply.func)
      args <- apply.args.traverse(evalExpr)
      ans  <- evalApp(func, args)
    } yield ans

  def evalApp(func: Value, args: List[Value])(implicit env: Env): F[Value] =
    func match {
      case closure : Closure => applyClosure(closure, args)
      case native  : Native  => applyNative(native, args)
      case value             => fail(s"Cannot call $value")
    }

  def evalInfix(infix: InfixExpr)(implicit env: Env): F[Value] =
    for {
      arg1 <- evalExpr(infix.arg1)
      arg2 <- evalExpr(infix.arg2)
      ans  <- applyNative(infixImpl(infix.op), List(arg1, arg2))
    } yield ans

  def evalPrefix(prefix: PrefixExpr)(implicit env: Env): F[Value] =
    for {
      arg  <- evalExpr(prefix.arg)
      ans  <- applyNative(prefixImpl(prefix.op), List(arg))
    } yield ans

  def evalFunc(func: FuncExpr)(implicit env: Env): F[Value] =
    pure(Closure(func, env))

  def evalBlock(block: BlockExpr)(implicit env0: Env): F[Value] = {
    val env1 = env0.push
    for {
      _    <- evalStmts(block.stmts)(env1)
      ans  <- evalExpr(block.expr)(env1)
    } yield ans
  }

  def evalStmts(stmts: List[Stmt])(implicit env: Env): F[Unit] =
    stmts.foldLeft(pure(()))((a, b) => a.flatMap(_ => evalStmt(b)))

  def evalStmt(stmt: Stmt)(implicit env: Env): F[Unit] =
    stmt match {
      case stmt: ExprStmt    => evalExprStmt(stmt)
      case stmt: LetStmt     => evalLetStmt(stmt)
      case stmt: LetTypeStmt => pure(())
    }

  def evalLetStmt(stmt: LetStmt)(implicit env: Env): F[Unit] =
    for {
      value <- evalExpr(stmt.expr)
      _     <- setVariable(stmt.varName, value)
    } yield ()

  def evalExprStmt(stmt: ExprStmt)(implicit env: Env): F[Unit] =
    evalExpr(stmt.expr).map(_ => ())

  def evalSelect(select: SelectExpr)(implicit env: Env): F[Value] =
    for {
      value  <- evalExpr(select)
      result <- evalSelect(value, select.field)
    } yield result

  def evalSelect(value: Value, id: String)(implicit env: Env): F[Value] =
    value match {
      case ObjVal(fields) =>
        fields.collectFirst { case (n, v) if n == id => v } match {
          case Some(value) => pure(value)
          case None        => fail(s"Field not found: $id")
        }

      case other =>
        fail(s"Could not select field '$id' from $other")
    }

  def evalCond(cond: CondExpr)(implicit env: Env): F[Value] =
    for {
      test   <- evalExpr(cond.test)
      result <- test match {
                  case BoolVal(true)  => evalExpr(cond.trueArm)
                  case BoolVal(false) => evalExpr(cond.falseArm)
                  case _              => fail(s"Conditional test was not a Boolean")
                }
    } yield result

  def evalObj(obj: ObjExpr)(implicit env: Env): F[Value] =
    obj.fields.traverse { case (n, e) => evalExpr(e).map(v => (n, v)) }.map(ObjVal)

  def evalArr(arr: ArrExpr)(implicit env: Env): F[Value] =
    arr.exprs.traverse(evalExpr).map(ArrVal)

  def applyClosure(closure: Closure, args: List[Value])(implicit env: Env): F[Value] = {
    val env1 = closure.env.push
    for {
      _   <- setVariables(closure.func.args.map(_.argName).zip(args))(env1)
      ans <- evalExpr(closure.func.body)(env1)
    } yield ans
  }

  def applyNative(native: Native, args: List[Value])(implicit env: Env): F[Value] =
    native.run(args)(this, env)

  // Environment helpers ------------------------

  def getVariable(name: String)(implicit env: Env): F[Value] =
    env.get(name) match {
      case Some(value) => pure(value)
      case None        => fail(s"Not in scope: $name")
    }

  def setVariable(name: String, value: Value)(implicit env: Env): F[Unit] =
    pure(env.destructiveSet(name, value))

  def setVariables(bindings: Seq[(String, Value)])(implicit env: Env): F[Unit] =
    pure(env.destructiveSetAll(bindings))

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
    try {
      pure(body)
    } catch { case NonFatal(exn) =>
      fail("Error executing native code", Some(exn))
    }
}
