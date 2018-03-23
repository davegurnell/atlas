package atlas

import cats.data.{EitherT, State}
import cats.implicits._

object Interpreter {
  type Step[A] = EitherT[Lambda[X => State[Env, X]], RuntimeError, A]

  def apply(expr: Expr, env: Env = Env.create): Either[RuntimeError, Value] =
    evalExpr(expr).value.runA(env).value

  def evalExpr(expr: Expr): Step[Value] =
    expr match {
      case expr: RefExpr    => evalRef(expr)
      case expr: LetExpr    => evalLet(expr)
      case expr: AppExpr    => evalApp(expr)
      case expr: InfixExpr  => evalInfix(expr)
      case expr: PrefixExpr => evalPrefix(expr)
      case expr: FuncExpr   => evalFunc(expr)
      case expr: BlockExpr  => evalBlock(expr)
      // case expr: Select  => evalSelect(expr)
      case expr: CondExpr   => evalCond(expr)
      case expr: CastExpr   => evalCast(expr)
      // case ObjExpr(fields)  => fields.traverse { case (n, e) => evalExpr(e).map(v => (n, v)) }.map(ObjVal)
      case ArrExpr(values)   => values.traverse(evalExpr).map(ArrVal)
      case StrExpr(value)    => pure(StrVal(value))
      case IntExpr(value)    => pure(IntVal(value))
      case DblExpr(value)    => pure(DblVal(value))
      case BoolExpr(value)   => pure(BoolVal(value))
      case NullExpr          => pure(NullVal)
    }

  def evalRef(ref: RefExpr): Step[Value] =
    for {
      env   <- currentEnv
      value <- env.get(ref.id).fold(fail[Value](s"Not in scope: ${ref.id}"))(pure)
    } yield value

  def evalLet(let: LetExpr): Step[Value] =
    for {
      value <- evalExpr(let.expr)
      _     <- inspectEnv(_.chain.destructiveSet(let.varName, value))
    } yield NullVal

  def evalApp(apply: AppExpr): Step[Value] =
    for {
      func <- evalExpr(apply.func)
      args <- apply.args.traverse(evalExpr)
      ans  <- evalAppInternal(func, args)
    } yield ans

  def evalInfix(infix: InfixExpr): Step[Value] =
    for {
      arg1 <- evalExpr(infix.arg1)
      arg2 <- evalExpr(infix.arg2)
      // ans  <- evalAppInternal(InfixImpl(infix.op), List(arg1, arg2))
    } yield ???

  def evalPrefix(prefix: PrefixExpr): Step[Value] =
    for {
      arg  <- evalExpr(prefix.arg)
      // ans  <- evalAppInternal(PrefixImpl(prefix.op), List(arg))
    } yield ???

  def evalFunc(func: FuncExpr): Step[Value] =
    inspectEnv(env => Closure(func, env) : Value)

  def evalBlock(block: BlockExpr): Step[Value] =
    pushScope {
      for {
        _    <- evalStmts(block.stmts)
        ans  <- evalExpr(block.expr)
      } yield ans
    }

  def evalStmts(stmts: List[Expr]): Step[Unit] =
    stmts.foldLeft(pure(())) { (a, b) =>
      a.flatMap(_ => evalStmt(b))
    }

  def evalStmt(stmt: Expr): Step[Unit] =
    evalExpr(stmt).map(_ => ())

  // def evalSelect(select: Select): Step[Value] =
  //   for {
  //     value  <- evalExpr(select)
  //     result <- evalSelect(value, select.field)
  //   } yield result

  // def evalSelect(value: Value, id: String): Step[Value] =
  //   value match {
  //     case ObjVal(fields) =>
  //       pureEither(fields.collectFirst { case (n, v) if n == id => v } match {
  //         case Some(value) => Right(value)
  //         case None        => Left(RuntimeError(s"Field not found: $id"))
  //       })

  //     case other =>
  //       fail(s"Could not select field '$id' from $other")
  //   }

  def evalCond(cond: CondExpr): Step[Value] =
    for {
      test   <- evalExpr(cond.test)
      result <- test match {
                  case BoolVal(true)  => evalExpr(cond.trueArm)
                  case BoolVal(false) => evalExpr(cond.falseArm)
                  case _              => fail(s"Conditional test was not a Boolean")
                }
    } yield result

  def evalCast(cast: CastExpr): Step[Value] =
    evalExpr(cast.expr)

  def evalAppInternal(func: Value, args: List[Value]): Step[Value] =
    func match {
      case closure : Closure => applyClosure(closure, args)
      // case native  : Native  => applyNative(native, args)
      case value             => fail(s"Cannot call $value")
    }

  def applyClosure(closure: Closure, args: List[Value]): Step[Value] =
    replaceEnv(closure.env) {
      pushScope {
        for {
          env <- currentEnv
          _    = env.destructiveSetAll(closure.func.args.map(_.argName).zip(args))
          ans <- evalExpr(closure.func.body)
        } yield ans
      }
    }

  // def applyNative(native: Native, args: List[Value]): Step[Value] =
  //   native.run(args)

  def pureEither[A](either: Either[RuntimeError, A]): Step[A] =
    EitherT(State[Env, Either[RuntimeError, A]](env => (env, either)))

  def pure[A](value: A): Step[A] =
    pureEither(Right(value))

  def tryCatch[A](value: => A): Step[A] =
   try pure(value) catch {
     case exn: Exception =>
       fail("Error in native function", Some(exn))
   }

  def fail[A](msg: String, cause: Option[Exception] = None): Step[A] =
    EitherT(State[Env, Either[RuntimeError, A]](env => (env, Left(RuntimeError(msg, cause)))))

  def inspectEnv[A](func: Env => A): Step[A] =
    EitherT(State[Env, Either[RuntimeError, A]](env => (env, Right(func(env)))))

  def inspectEnvEither[A](func: Env => Either[RuntimeError, A]): Step[A] =
    EitherT(State[Env, Either[RuntimeError, A]](env => (env, func(env))))

  val currentEnv: Step[Env] =
    EitherT(State[Env, Either[RuntimeError, Env]](env => (env, Right(env))))

  def modifyEnv(f: Env => Env): Step[Unit] =
    EitherT(State[Env, Either[RuntimeError, Unit]](env => (f(env), Right(()))))

  def replaceEnv[A](env: Env)(body: Step[A]): Step[A] =
    for {
      env0 <- currentEnv
      _    <- modifyEnv(_ => env)
      ans  <- body
      _    <- modifyEnv(_ => env0)
    } yield ans

  def pushScope[A](body: Step[A]): Step[A] =
    for {
      _   <- modifyEnv(_.push)
      ans <- body
      _   <- modifyEnv(_.pop)
    } yield ans
}
