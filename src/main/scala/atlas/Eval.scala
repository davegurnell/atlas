package atlas

import cats.data.{EitherT, State}
import cats.implicits._

object Eval {
  import Ast._

  final case class Error(text: String)

  type Step[A] = EitherT[Lambda[X => State[Env, X]], Error, A]

  def apply(top: TopLevel, env: Env, ref: Ref = Ref("main")): Either[Error, Value] =
    evalTopLevel(top, ref).value.runA(env).value

  def apply(expr: Expr, env: Env): Either[Error, Value] =
    evalExpr(expr).value.runA(env).value

  def evalTopLevel(top: TopLevel, ref: Ref): Step[Value] =
    for {
      _   <- evalStmts(top.stmts)
      ans <- evalExpr(ref)
    } yield ans

  def evalStmts(stmts: List[Stmt]): Step[Unit] =
    stmts.foldLeft(pure(()))((a, b) => a.flatMap(_ => evalStmt(b)))

  def evalStmt(stmt: Stmt): Step[Unit] =
    stmt match {
      case stmt: Defn => evalDefn(stmt)
      case expr: Expr => evalExpr(expr).map(_ => ())
    }

  def evalDefn(defn: Defn): Step[Unit] =
    for {
      value <- evalExpr(defn.expr)
      _     <- modifyScope(_.set(defn.ref.id, value))
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
    inspectScopeEither(_.get(ref.id).toRight(Error(s"Not in scope: ${ref.id}")))

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
      case expr: FuncLiteral  => evalFunc(expr)
    }

  def evalFunc(func: FuncLiteral): Step[Value] =
    inspectScope(env => BoundFunc(func, env) : Value)

  def evalBlock(block: Block): Step[Value] =
    for {
      _   <- evalStmts(block.stmts)
      ans <- evalExpr(block.expr)
    } yield ans

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
      case bound  : BoundFunc  => applyBoundFunc(bound, args)
      case native : NativeFunc => applyNativeFunc(native, args)
      case value  : DataValue  => fail(s"Cannot call non-function: $value")
    }

  def applyBoundFunc(bound: BoundFunc, args: List[Value]): Step[Value] =
    for {
      origScope <- currentScope
      _         <- checkArity(bound.func.args.length, args.length)
      _         <- replaceScope(bound.env.setAll(bound.func.args.map(_.id).zip(args)))
      result    <- evalExpr(bound.func.body)
      _         <- replaceScope(origScope)
    } yield result

  def applyNativeFunc(native: NativeFunc, args: List[Value]): Step[Value] =
    for {
      _      <- checkArity(native.arity, args.length)
      result <- pureEither(native.func(args).leftMap(Error))
    } yield result

  def checkArity(arity: Int, args: Int): Step[Unit] =
    if(arity == args) {
      pure(())
    } else {
      fail(s"Arity mismatch: called $arity function with $args parameters")
    }

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

  def inspectScope[A](func: Env => A): Step[A] =
    EitherT(State[Env, Either[Error, A]](env => (env, Right(func(env)))))

  def inspectScopeEither[A](func: Env => Either[Error, A]): Step[A] =
    EitherT(State[Env, Either[Error, A]](env => (env, func(env))))

  val currentScope: Step[Env] =
    EitherT(State[Env, Either[Error, Env]](env => (env, Right(env))))

  def modifyScope(f: Env => Env): Step[Unit] =
    EitherT(State[Env, Either[Error, Unit]](env => (f(env), Right(()))))

  def replaceScope(env: Env): Step[Unit] =
    modifyScope(_ => env)
}
