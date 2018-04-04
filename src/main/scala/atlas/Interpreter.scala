package atlas

import atlas.SyncInterpreter.Step
import cats.{Eval, MonadError}
import cats.data.{EitherT, State, StateT}
import cats.implicits._
import scala.util.control.NonFatal
import scala.concurrent.{Future, ExecutionContext}

object SyncInterpreter extends Interpreter[EitherT[Eval, RuntimeError, ?]] {
  type F[A] = EitherT[Eval, RuntimeError, A]
}

class AsyncInterpreter(implicit ec: ExecutionContext) extends Interpreter[EitherT[Future, RuntimeError, ?]] {
  type F[A] = EitherT[Future, RuntimeError, A]
}

abstract class Interpreter[F[_]](implicit val monad: MonadError[F, RuntimeError])
  extends InterpreterBoilerplate[F]
  with InfixImpl[F]
  with PrefixImpl[F]
  with NativeImpl[F] {

  /** Wrap F[_] in a state monad to track IState */
  type Step[A] = EvalStep[F, A]

  /**
   * Helpers for creating Native functions:
   *
   * - apply(func) - creates a Native from a function (A, B, ...) => R
   * - applyF(func) - creates a Native from a function (A, B, ...) => F[R]
   */
  object native extends NativeFunctions

  /**
   * ValueEncoders/ValueDecoders for functions.
   * These encode/decoder functions of the form (A, B, ...) => F[R].
   */
  object implicits extends NativeEncoders with NativeDecoders

  def apply(expr: Expr, env: Env[F] = createEnv): F[Value[F]] =
    evalExpr(expr).runA(env)

  def evalExpr(expr: Expr): Step[Value[F]] =
    monitoringChecks {
      expr match {
        case expr: RefExpr    => evalRef(expr)
        case expr: AppExpr    => evalApp(expr)
        case expr: InfixExpr  => evalInfix(expr)
        case expr: PrefixExpr => evalPrefix(expr)
        case expr: FuncExpr   => evalFunc(expr)
        case expr: BlockExpr  => evalBlock(expr)
        case expr: SelectExpr => evalSelect(expr)
        case expr: CondExpr   => evalCond(expr)
        case ParenExpr(expr)  => evalExpr(expr)
        case ObjExpr(fields)  => fields.traverse { case (n, e) => evalExpr(e).map(v => (n, v)) }.map(ObjVal.apply[F])
        case ArrExpr(values)  => values.traverse(evalExpr).map(ArrVal.apply[F])
        case StrExpr(value)   => pure(StrVal(value))
        case IntExpr(value)   => pure(IntVal(value))
        case DblExpr(value)   => pure(DblVal(value))
        case BoolExpr(value)  => pure(BoolVal(value))
        case NullExpr         => pure(NullVal())
      }
    }

  def evalRef(ref: RefExpr): Step[Value[F]] =
    for {
      env   <- currentEnv
      value <- env.get(ref.id).fold(fail[Value[F]](s"Not in scope: ${ref.id}"))(pure)
    } yield value

  def evalApp(apply: AppExpr): Step[Value[F]] =
    for {
      func <- evalExpr(apply.func)
      args <- apply.args.traverse(evalExpr)
      ans  <- evalAppInternal(func, args)
    } yield ans

  def evalAppInternal(func: Value[F], args: List[Value[F]]): Step[Value[F]] =
    func match {
      case closure : Closure[F] => applyClosure(closure, args)
      case native  : Native[F]  => applyNative(native, args)
      case value                => fail(s"Cannot call $value")
    }

  def evalInfix(infix: InfixExpr): Step[Value[F]] =
    for {
      arg1 <- evalExpr(infix.arg1)
      arg2 <- evalExpr(infix.arg2)
      ans  <- applyNative(infixImpl(infix.op), List(arg1, arg2))
    } yield ans

  def evalPrefix(prefix: PrefixExpr): Step[Value[F]] =
    for {
      arg  <- evalExpr(prefix.arg)
      ans  <- applyNative(prefixImpl(prefix.op), List(arg))
    } yield ans

  def evalFunc(func: FuncExpr): Step[Value[F]] =
    inspectEnv(env => Closure(func, env) : Value[F])

  def evalBlock(block: BlockExpr): Step[Value[F]] =
    pushScope {
      for {
        _    <- evalStmts(block.stmts)
        ans  <- evalExpr(block.expr)
      } yield ans
    }

  def evalStmts(stmts: List[Stmt]): Step[Unit] =
    stmts.foldLeft(pure(())) { (a, b) =>
      a.flatMap(_ => evalStmt(b))
    }

  def evalStmt(stmt: Stmt): Step[Unit] =
    stmt match {
      case stmt: LetStmt  => evalLetStmt(stmt)
      case stmt: ExprStmt => evalExprStmt(stmt)
    }

  def evalLetStmt(stmt: LetStmt): Step[Unit] =
    for {
      value <- evalExpr(stmt.expr)
      _     <- inspectEnv(_.chain.destructiveSet(stmt.varName, value))
    } yield NullVal()

  def evalExprStmt(stmt: ExprStmt): Step[Unit] =
    evalExpr(stmt.expr).map(_ => ())

  def evalSelect(select: SelectExpr): Step[Value[F]] =
    for {
      value  <- evalExpr(select)
      result <- evalSelect(value, select.field)
    } yield result

  def evalSelect(value: Value[F], id: String): Step[Value[F]] =
    value match {
      case ObjVal(fields) =>
        fields.collectFirst { case (n, v) if n == id => v } match {
          case Some(value) => value.pure[Step]
          case None        => RuntimeError(s"Field not found: $id").raiseError[Step, Value[F]]
        }

      case other =>
        fail(s"Could not select field '$id' from $other")
    }

  def evalCond(cond: CondExpr): Step[Value[F]] =
    for {
      test   <- evalExpr(cond.test)
      result <- test match {
                  case BoolVal(true)  => evalExpr(cond.trueArm)
                  case BoolVal(false) => evalExpr(cond.falseArm)
                  case _              => fail(s"Conditional test was not a Boolean")
                }
    } yield result

  def applyClosure(closure: Closure[F], args: List[Value[F]]): Step[Value[F]] =
    replaceEnv(closure.env) {
      pushScope {
        for {
          env <- currentEnv
          _    = env.destructiveSetAll(closure.func.args.map(_.argName).zip(args))
          ans <- evalExpr(closure.func.body)
        } yield ans
      }
    }

  def applyNative(native: Native[F], args: List[Value[F]]): Step[Value[F]] =
    native(args)

  val currentEnv: Step[Env[F]] =
    inspectEnv(identity)

  def pushScope[A](body: Step[A]): Step[A] =
    for {
      _   <- modifyEnv(_.push)
      ans <- body
      _   <- modifyEnv(_.pop)
    } yield ans

  def replaceEnv[A](env: Env[F])(body: Step[A]): Step[A] =
    for {
      env0 <- currentEnv
      _    <- modifyEnv(_ => env)
      ans  <- body
      _    <- modifyEnv(_ => env0)
    } yield ans

  def pure[A](value: A): Step[A] =
    pureF(value.pure[F])

  def pureF[A](fa: F[A]): Step[A] =
    StateT.apply((env: Env[F]) => fa.map(a => (env, a)))

  def fail[A](msg: String, cause: Option[Exception] = None): Step[A] =
    RuntimeError(msg, cause).raiseError[Step, A]

  def inspectEnv[A](func: Env[F] => A): Step[A] =
    inspectEnvF(env => func(env).pure[F])

  def inspectEnvF[A](func: Env[F] => F[A]): Step[A] =
    StateT.inspectF(func)

  def modifyEnv(func: Env[F] => Env[F]): Step[Unit] =
    modifyEnvF(env => func(env).pure[F])

  def modifyEnvF(func: Env[F] => F[Env[F]]): Step[Unit] =
    StateT.modifyF(func)

  def catchNonFatal[A](body: => A): Step[A] =
    StateT.liftF {
      try {
        body.pure[F]
      } catch {
        case NonFatal(exn) =>
          RuntimeError("Error executing native code", Some(exn)).raiseError[F, A]
      }
    }

  def monitoringChecks[A](body: => Step[A]): Step[A] =
    body
}
