package atlas

import cats.data.{EitherT, State}
import cats.implicits._

object TypeChecker {
  import Ast._
  import Ast.Literal._

  type Step[A] = EitherT[Lambda[X => State[TypeEnv, X]], TypeError, A]

  def apply(expr: Expr, env: Env = Env.create): Either[TypeError, Type] =
    exprType(expr).value.runA(typeEnv(env)).value

  def exprType(expr: Expr): Step[Type] =
    expr match {
      case expr: Ref     => refType(expr)
      case expr: Block   => blockType(expr)
      case expr: Select  => selectType(expr)
      case expr: Cond    => condType(expr)
      case expr: Infix   => infixType(expr)
      case expr: Prefix  => prefixType(expr)
      case expr: Cast    => castType(expr)
      case expr: Apply   => applyType(expr)
      case expr: Literal => literalType(expr)
    }

  def refType(ref: Ref): Step[Type] =
    lookupType(ref)

  def blockType(block: Block): Step[Type] =
    pushScope {
      for {
        _    <- stmtsType(block.stmts)
        ans  <- exprType(block.expr)
      } yield ans
    }

  def stmtsType(stmts: List[Stmt]): Step[Unit] =
    stmts.foldLeft(pure(())) { (a, b) =>
      a.flatMap(_ => stmtType(b))
    }

  def stmtType(stmt: Stmt): Step[Unit] =
    stmt match {
      case defn: DefnStmt => defnType(defn)
      case stmt: ExprStmt => exprType(stmt.expr).map(_ => ())
    }

  def defnType(defn: DefnStmt): Step[Unit] =
    for {
      exprTpe <- exprType(defn.expr)
      defnTpe <- defn.tpe match {
                   case Some(defnTpe) => assertAssignable(defnTpe, exprTpe).map(_ => defnTpe)
                   case None          => pure(exprTpe)
                 }
      _       <- inspectEnv(_.scopes.head.destructiveSet(Ref(defn.name), defnTpe))
    } yield ()

  def selectType(expr: Select): Step[Type] =
    ???

  def condType(expr: Cond): Step[Type] =
    for {
      a <- exprType(expr.test)
      _ <- assertAssignable(a, Type.Bool)
      b <- exprType(expr.trueArm)
      c <- exprType(expr.falseArm)
    } yield Type.union(b, c)

  def infixType(expr: Infix): Step[Type] =
    for {
      a   <- exprType(expr.arg1)
      b   <- exprType(expr.arg2)
      ans <- infixType(expr.op, a, b)
    } yield ans

  def infixType(op: InfixOp, a: Type, b: Type): Step[Type] = {
    import InfixOp._
    import Type._

    // TODO: Replace with lookup in InfixFunc:
    (op, a, b) match {
      case (Add, Intr, Intr) => pure(Intr)
      case (Add, Intr, Real) => pure(Real)
      case (Add, Real, Intr) => pure(Real)
      case (Add, Real, Real) => pure(Real)
      case (Add, Str,  Str)  => pure(Str)

      case (Sub, Intr, Intr) => pure(Intr)
      case (Sub, Real, Intr) => pure(Real)
      case (Sub, Intr, Real) => pure(Real)
      case (Sub, Real, Real) => pure(Real)

      case (Mul, Intr, Intr) => pure(Intr)
      case (Mul, Real, Intr) => pure(Real)
      case (Mul, Intr, Real) => pure(Real)
      case (Mul, Real, Real) => pure(Real)

      case (Div, Intr, Intr) => pure(Intr)
      case (Div, Real, Intr) => pure(Real)
      case (Div, Intr, Real) => pure(Real)
      case (Div, Real, Real) => pure(Real)

      case (And, Bool, Bool) => pure(Bool)
      case (Or,  Bool, Bool) => pure(Bool)

      case (Eq,  a, b) if a == b => pure(Bool)
      case (Ne,  a, b) if a == b => pure(Bool)

      case (Gt,  Intr, Intr) => pure(Bool)
      case (Gt,  Intr, Real) => pure(Bool)
      case (Gt,  Real, Intr) => pure(Bool)
      case (Gt,  Real, Real) => pure(Bool)
      case (Gt,  Str,  Str)  => pure(Bool)
      case (Gt,  Bool, Bool) => pure(Bool)

      case (Lt,  Intr, Intr) => pure(Bool)
      case (Lt,  Intr, Real) => pure(Bool)
      case (Lt,  Real, Intr) => pure(Bool)
      case (Lt,  Real, Real) => pure(Bool)
      case (Lt,  Str,  Str)  => pure(Bool)
      case (Lt,  Bool, Bool) => pure(Bool)

      case (Gte, Intr, Intr) => pure(Bool)
      case (Gte, Intr, Real) => pure(Bool)
      case (Gte, Real, Intr) => pure(Bool)
      case (Gte, Real, Real) => pure(Bool)
      case (Gte, Str,  Str)  => pure(Bool)
      case (Gte, Bool, Bool) => pure(Bool)

      case (Lte, Intr, Intr) => pure(Bool)
      case (Lte, Intr, Real) => pure(Bool)
      case (Lte, Real, Intr) => pure(Bool)
      case (Lte, Real, Real) => pure(Bool)
      case (Lte, Str,  Str)  => pure(Bool)
      case (Lte, Bool, Bool) => pure(Bool)

      case _ => fail(InfixNotDefined(op, a, b))
    }
  }

  def prefixType(expr: Prefix): Step[Type] =
    for {
      a   <- exprType(expr.arg)
      ans <- prefixType(expr.op, a)
    } yield ans

  def prefixType(op: PrefixOp, a: Type): Step[Type] = {
    import PrefixOp._
    import Type._

    // TODO: Replace with lookup in PrefixFunc:
    (op, a) match {
      case (Not, Bool) => pure(Bool)

      case (Pos, Intr) => pure(Intr)
      case (Pos, Real) => pure(Real)

      case (Neg, Intr) => pure(Intr)
      case (Neg, Real) => pure(Real)

      case _ => fail(PrefixNotDefined(op, a))
    }
  }

  def castType(cast: Cast): Step[Type] =
    for {
      exprTpe <- exprType(cast.expr)
      _       <- assertAssignable(cast.asType, exprTpe)
    } yield cast.asType

  def applyType(expr: Apply): Step[Type] =
    ???

  def literalType(expr: Literal): Step[Type] =
    expr match {
      case expr: Func  => funcType(expr)
      case Obj(fields) => fields
                            .traverse { case (n, e) => exprType(e).map(t => Map(n -> t)) }
                            .map(ts => Type.Obj(ts.combineAll.toList))
      case Arr(items)  => items
                            .traverse(exprType)
                            .map(_.combineAll)
      case Str(value)  => pure(Type.Str)
      case Intr(value) => pure(Type.Intr)
      case Real(value) => pure(Type.Real)
      case Null        => pure(Type.Null)
      case _: Bool     => pure(Type.Bool)
    }

  def funcType(func: Func): Step[Type] =
    for {
      argTypes <- func.args.traverse(argType)
      exprType <- exprType(func.body)
      resType  <- func.resultType match {
                    case Some(tpe) => assertAssignable(tpe, exprType).map(_ => tpe)
                    case _         => pure(exprType)
                  }
    } yield Type.Func(argTypes, resType)

  def argType(arg: Arg): Step[Type] =
    arg.tpe match {
      case Some(tpe) => assignType(Ref(arg.name), tpe)
      case None      => generateType(Ref(arg.name))
    }

  // Applying types to initial native environments:

  def typeEnv(env: Env): TypeEnv = {
    TypeEnv(env.scopes.map(typeScope))
  }

  def typeScope(scope: Scope): TypeScope =
    TypeScope(scope.bindings.map { case (n, v) => (Ref(n), TypeChecker.valueType(v)) }, Set())

  def valueType(value: Value): Type =
    value match {
      case Value.Obj(fields)   => Type.Obj(fields.map { case (n, v) => (n, valueType(v)) })
      case Value.Arr(items)    => Type.Arr(items.foldLeft(Type.bottom)((t, v) => Type.union(t, valueType(v))))
      case _: Value.Str        => Type.Str
      case _: Value.Intr       => Type.Intr
      case _: Value.Real       => Type.Real
      case _: Value.Bool       => Type.Bool
      case Value.Null          => Type.Null
      case Value.Closure(f, _) => ???
      case Value.Native(t, _)  => t
    }

  // Underlying step constructors:

  def pureEither[A](either: Either[TypeError, A]): Step[A] =
    EitherT(State[TypeEnv, Either[TypeError, A]](types => (types, either)))

  def pure[A](value: A): Step[A] =
    pureEither(Right(value))

  def fail[A](error: TypeError): Step[A] =
    EitherT(State[TypeEnv, Either[TypeError, A]](env => (env, Left(error))))

  def assertAssignable(to: Type, from: Type): Step[Unit] =
    (to, from) match {
      case (a, b) if a == b       => pure(())
      case (Type.Real, Type.Intr) => pure(())
      case (a, b)                 => fail(TypeMismatch(a, b))
    }

  def inspectEnv[A](func: TypeEnv => A): Step[A] =
    EitherT(State[TypeEnv, Either[TypeError, A]](env => (env, Right(func(env)))))

  def currentEnv: Step[TypeEnv] =
    EitherT(State[TypeEnv, Either[TypeError, TypeEnv]](env => (env, Right(env))))

  def inspectEnvEither[A](func: TypeEnv => Either[TypeError, A]): Step[A] =
    EitherT(State[TypeEnv, Either[TypeError, A]](env => (env, func(env))))

  def modifyEnv(f: TypeEnv => TypeEnv): Step[Unit] =
    EitherT(State[TypeEnv, Either[TypeError, Unit]](env => (f(env), Right(()))))

  def lookupType(expr: Expr): Step[Type] =
    currentEnv.flatMap { env =>
      env.get(expr) match {
        case Some(tpe) => pure(tpe)
        case None      => generateType(expr)
      }
    }

  def assignType(expr: Expr, tpe: Type): Step[Type] =
    modifyEnv(env => env.set(expr, tpe)).map(_ => tpe)

  def generateType(expr: Expr): Step[Type] = {
    val tpe = TypeEnv.gen
    modifyEnv(env  => env.set(expr, tpe)).map(_ => tpe)
  }

  def pushScope[A](body: Step[A]): Step[A] =
    for {
      _   <- modifyEnv(_.push)
      ans <- body
      _   <- modifyEnv(_.pop)
    } yield ans
}