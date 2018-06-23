package atlas

import cats.data.{EitherT, StateT}
import cats.implicits._
import cats.{Eval, MonadError}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

object Interpreter {
  val sync: Interpreter[EitherT[Eval, RuntimeError, ?]] =
    new Interpreter[EitherT[Eval, RuntimeError, ?]]

  def async(implicit ec: ExecutionContext): Interpreter[EitherT[Future, RuntimeError, ?]] =
    new Interpreter[EitherT[Future, RuntimeError, ?]]
}

class Interpreter[F[_]](implicit val monad: MonadError[F, RuntimeError])
  extends InterpreterBoilerplate[F]
  with InfixImpl[F]
  with PrefixImpl[F]
  with NativeImpl[F] {

  /** Alias for F[_] so we can refer to it outside the interpreter */
  type Out[A] = F[A]

  /** Wrap F[_] in a state monad to track interpreter state */
  type Step[A] = EvalStep[F, A]

  /**
   * Helpers for creating Native functions:
   * - apply(func) - creates a Native from a function (A, B, ...) => R
   * - pure(func) - creates a Native from a function (A, B, ...) => Step[R]
   */
  object native extends NativeFunctions

  /**
   * ValueEncoders/ValueDecoders for functions.
   * These encode/decoder functions of the form (A, B, ...) => Step[R].
   */
  object implicits extends NativeEncoders with NativeDecoders

  def env: Env[F] =
    ScopeChain
      .create[String, Value[F]]
      .setAll(infixBindings)
      .setAll(prefixBindings)
      .setAll(basicBindings)

  def evalAs[A](expr: Expr, env: Env[F] = env, limits: Limits = Limits.create)(implicit dec: ValueDecoder[F, A]): F[A] =
    eval(expr, env, limits).flatMap(dec.apply)

  def eval(expr: Expr, env: Env[F] = env, limits: Limits = Limits.create): F[Value[F]] =
    evalExpr(expr).runA((env, limits))

  def evalExpr(expr: Expr): Step[Value[F]] =
    monitoringChecks {
      expr match {
        case expr: RefExpr     => evalRef(expr)
        case expr: AppExpr     => evalApp(expr)
        case expr: FuncExpr    => evalFunc(expr)
        case expr: BlockExpr   => evalBlock(expr)
        case expr: SelectExpr  => evalSelect(expr)
        case expr: CondExpr    => evalCond(expr)
        case CastExpr(expr, _) => evalExpr(expr)
        case expr: ObjExpr     => evalObj(expr)
        case expr: ArrExpr     => evalArr(expr)
        case StrExpr(value)    => pure(StrVal(value))
        case IntExpr(value)    => pure(IntVal(value))
        case DblExpr(value)    => pure(DblVal(value))
        case BoolExpr(value)   => pure(BoolVal(value))
        case NullExpr          => pure(NullVal())
      }
    }

  def evalRef(ref: RefExpr): Step[Value[F]] =
    getVariable(ref.name)

  def evalApp(apply: AppExpr): Step[Value[F]] =
    for {
      func <- evalExpr(apply.func)
      args <- apply.args.traverse(evalExpr)
      ans  <- evalApp(func, args)
    } yield ans

  def evalApp(func: Value[F], args: List[Value[F]]): Step[Value[F]] =
    func match {
      case closure : Closure[F] => applyClosure(closure, args)
      case native  : Native[F]  => applyNative(native, args)
      case value                => fail(s"Cannot call $value")
    }

  def evalFunc(func: FuncExpr): Step[Value[F]] =
    createClosure(func)

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
      case stmt: ExprStmt => evalExprStmt(stmt)
      case stmt: LetStmt  => evalLetStmt(stmt)
      case stmt: TypeStmt => pure(())
    }

  def evalLetStmt(stmt: LetStmt): Step[Unit] =
    for {
      value <- evalExpr(stmt.expr)
      _     <- setVariable(stmt.name, value)
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

  def evalObj(obj: ObjExpr): Step[Value[F]] =
    obj.fields.traverse { case (n, e) => evalExpr(e).map(v => (n, v)) }.map(ObjVal[F])

  def evalArr(arr: ArrExpr): Step[Value[F]] =
    arr.exprs.traverse(evalExpr).map(ArrVal[F])

  def applyClosure(closure: Closure[F], args: List[Value[F]]): Step[Value[F]] =
    swapEnv(closure.env) {
      pushScope {
        for {
          env <- setVariables(closure.func.args.map(_.name).zip(args))
          ans <- evalExpr(closure.func.body)
        } yield ans
      }
    }

  def applyNative(native: Native[F], args: List[Value[F]]): Step[Value[F]] =
    native(args)

  // Environment helpers ------------------------

  def getVariable(name: String): Step[Value[F]] =
    inspectEnv(env => env.get(name) match {
      case Some(value) => value.pure[F]
      case None        => RuntimeError(s"Not in scope: $name").raiseError[F, Value[F]]
    })

  def setVariable(name: String, value: Value[F]): Step[Unit] =
    inspectEnv(env => env.destructiveSet(name, value).pure[F])

  def setVariables(bindings: Seq[(String, Value[F])]): Step[Unit] =
    inspectEnv(env => env.destructiveSetAll(bindings).pure[F])

  def createClosure(func: FuncExpr): Step[Value[F]] =
    inspectEnv(env => (Closure(func, env) : Value[F]).pure[F])

  val currentEnv: Step[Env[F]] =
    inspectEnv(env => env.pure[F])

  def swapEnv[A](env: Env[F])(body: Step[A]): Step[A] =
    for {
      env0 <- currentEnv
      _    <- updateEnv(_ => env.pure[F])
      ans  <- body
      _    <- updateEnv(_ => env0.pure[F])
    } yield ans

  def pushScope[A](body: Step[A]): Step[A] =
    for {
      _   <- updateEnv(_.push.pure[F])
      ans <- body
      _   <- updateEnv(_.pop.pure[F])
    } yield ans

  def inspectEnv[A](func: Env[F] => F[A]): Step[A] =
    StateT.inspectF { case (env, limits) => func(env) }

  def updateEnv(func: Env[F] => F[Env[F]]): Step[Unit] =
    StateT.modifyF { case (env, limits) => func(env).map(env => (env, limits)) }

  // Monitoring helpers -------------------------

  def monitoringChecks[A](body: => Step[A]): Step[A] =
    checkLimits.flatMap(_ => body)

  def checkLimits: Step[Unit] =
    StateT.modifyF {
      case (env, limits) =>
        def pcFailure: Option[F[(Env[F], Limits)]] =
          limits.pcLimit.collect {
            case limit if limit < limits.pc =>
              RuntimeError(s"Script exceeded program counter limit: $limit").raiseError[F, (Env[F], Limits)]
          }

        def runtimeFailure: Option[F[(Env[F], Limits)]] =
          limits.runtimeLimit.collect {
            case limit if limit < (System.currentTimeMillis - limits.startTime) =>
              RuntimeError(s"Script exceeded runtime limit: ${limit}ms").raiseError[F, (Env[F], Limits)]
          }

        def success: F[(Env[F], Limits)] =
          (env, limits.copy(pc = limits.pc + 1)).pure[F]

        pcFailure orElse runtimeFailure getOrElse success
    }

  def inspectLimits[A](func: Limits => F[A]): Step[A] =
    StateT.inspectF { case (env, limits) => func(limits) }

  def updateLimits(func: Limits => F[Limits]): Step[Unit] =
    StateT.modifyF { case (env, limits) => func(limits).map(limits => (env, limits)) }

  // Error handling helpers ---------------------

  def pure[A](value: A): Step[A] =
    StateT(env => (env, value).pure[F])

  def fail[A](msg: String, cause: Option[Exception] = None): Step[A] =
    RuntimeError(msg, cause).raiseError[Step, A]

  def catchNonFatal[A](body: => A): Step[A] =
    StateT.liftF {
      try {
        body.pure[F]
      } catch {
        case NonFatal(exn) =>
          RuntimeError("Error executing native code", Some(exn)).raiseError[F, A]
      }
    }
}
