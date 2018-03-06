package atlas

import cats.data.{EitherT, State}
import cats.implicits._

object Eval {
  import Ast._

  final case class Error(text: String)

  type Step[A] = EitherT[Lambda[X => State[Env, X]], Error, A]

  def apply(expr: Expr, env: Env = Env.create): Either[Error, Value] =
    evalExpr(expr).value.runA(env).value

  def evalBlock(block: Block): Step[Value] =
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
      case defn: Defn => evalDefn(defn)
      case expr: Expr => evalExpr(expr).map(_ => ())
    }

  def evalDefn(defn: Defn): Step[Unit] =
    for {
      value <- evalExpr(defn.expr)
      _     <- inspectEnv(_.scopes.head.destructiveSet(defn.ref.id, value))
    } yield ()

  def evalExpr(expr: Expr): Step[Value] =
    expr match {
      case expr: Ref     => evalRef(expr)
      case expr: Literal => evalLiteral(expr)
      case expr: Block   => evalBlock(expr)
      case expr: Select  => evalSelect(expr)
      case expr: Cond    => evalCond(expr)
      case expr: Prefix  => evalPrefix(expr)
      case expr: Infix   => evalInfix(expr)
      case expr: Apply   => evalApply(expr)
    }

  def evalRef(ref: Ref): Step[Value] =
    for {
      env   <- currentEnv
      value <- env.get(ref.id).fold(fail[Value](s"Not in scope: ${ref.id}"))(pure)
    } yield value

  def evalSelect(select: Select): Step[Value] =
    for {
      value  <- evalExpr(select)
      result <- selectValue(value, select.ref.id)
    } yield result

  def evalLiteral(lit: Literal): Step[Value] =
    lit match {
      case NullLiteral         => pure(NullValue)
      case TrueLiteral         => pure(TrueValue)
      case FalseLiteral        => pure(FalseValue)
      case expr: IntLiteral    => pure(IntValue(expr.value))
      case expr: DoubleLiteral => pure(DoubleValue(expr.value))
      case expr: StringLiteral => pure(StringValue(expr.value))
      case expr: ArrayLiteral  => expr.items.traverse(evalExpr).map(ArrayValue)
      case expr: ObjectLiteral => expr.fields.traverse { case (n, e) => evalExpr(e).map(v => (n, v)) }.map(ObjectValue)
      case expr: FuncLiteral   => evalFunc(expr)
    }

  def evalFunc(func: FuncLiteral): Step[Value] =
    inspectEnv(env => Closure(func, env) : Value)

  def evalCond(cond: Cond): Step[Value] =
    for {
      test   <- evalExpr(cond.test).flatMap(parseAs[Boolean])
      result <- if(test) evalExpr(cond.trueArm) else evalExpr(cond.falseArm)
    } yield result

  def evalPrefix(prefix: Prefix): Step[Value] =
    for {
      arg  <- evalExpr(prefix.arg)
      ans  <- evalApplyInternal(PrefixFunc(prefix.op), List(arg))
    } yield ans

  def evalInfix(infix: Infix): Step[Value] =
    for {
      arg1 <- evalExpr(infix.arg1)
      arg2 <- evalExpr(infix.arg2)
      ans  <- evalApplyInternal(InfixFunc(infix.op), List(arg1, arg2))
    } yield ans

  def evalApply(apply: Apply): Step[Value] =
    for {
      func <- evalExpr(apply.func)
      args <- apply.args.traverse(evalExpr)
      ans  <- evalApplyInternal(func, args)
    } yield ans

  def evalApplyInternal(func: Value, args: List[Value]): Step[Value] =
    func match {
      case closure : Closure    => applyClosure(closure, args)
      case native  : NativeFunc => applyNativeFunc(native, args)
      case value   : DataValue  => fail(s"Cannot call non-function: $value")
    }

  def applyClosure(closure: Closure, args: List[Value]): Step[Value] =
    replaceEnv(closure.env) {
      pushScope {
        for {
          env <- currentEnv
          _    = env.scopes.head.destructiveSetAll(closure.func.args.map(_.id).zip(args))
          ans <- evalExpr(closure.func.body)
        } yield ans
      }
    }

  def applyNativeFunc(native: NativeFunc, args: List[Value]): Step[Value] =
    pureEither(native.func(args).leftMap(Error))

  def selectValue(value: Value, id: String): Step[Value] =
    value match {
      case ObjectValue(fields) =>
        pureEither(fields.collectFirst { case (n, v) if n == id => v } match {
          case Some(value) => Right(value)
          case None        => Left(Error(s"Field not found: $id"))
        })
      case other =>
        fail(s"Could not select field '$id' from $other")
    }

  def parseAs[A](value: Value)(implicit dec: ValueDecoder[A]): Step[A] =
    pureEither(dec(value).leftMap(Error))

  def pureEither[A](either: Either[Error, A]): Step[A] =
    EitherT(State[Env, Either[Error, A]](env => (env, either)))

  def pure[A](value: A): Step[A] =
    pureEither(Right(value))

  def fail[A](msg: String): Step[A] =
    EitherT(State[Env, Either[Error, A]](env => (env, Left(Error(msg)))))

  def inspectEnv[A](func: Env => A): Step[A] =
    EitherT(State[Env, Either[Error, A]](env => (env, Right(func(env)))))

  def inspectEnvEither[A](func: Env => Either[Error, A]): Step[A] =
    EitherT(State[Env, Either[Error, A]](env => (env, func(env))))

  val currentEnv: Step[Env] =
    EitherT(State[Env, Either[Error, Env]](env => (env, Right(env))))

  def modifyEnv(f: Env => Env): Step[Unit] =
    EitherT(State[Env, Either[Error, Unit]](env => (f(env), Right(()))))

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
