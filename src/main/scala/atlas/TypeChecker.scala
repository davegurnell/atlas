// package atlas

// import cats.data.{EitherT, State}
// import cats.implicits._

// object TypeChecker {
//   type Step[A] = EitherT[Lambda[X => State[TypeEnv, X]], TypeError, A]

//   def apply(expr: Expr, env: Env = Env.create): Either[TypeError, Type] =
//     exprType(expr).value.runA(typeEnv(env)).value

//   def exprType(expr: Expr): Step[Type] =
//     expr match {
//       case expr: RefExpr     => refType(expr)
//       case expr: LetExpr     => letType(expr)
//       case expr: AppExpr     => appType(expr)
//       case expr: InfixExpr   => infixType(expr)
//       case expr: PrefixExpr  => prefixType(expr)
//       case expr: FuncExpr    => funcType(expr)
//       case expr: BlockExpr   => blockType(expr)
//       // case expr: SelectExpr  => selectType(expr)
//       case expr: CondExpr    => condType(expr)
//       case expr: CastExpr    => castType(expr)
//       // case expr: ObjExpr     => objType(expr)
//       // case expr: ArrExpr     => arrType(expr)
//       case expr: StrExpr     => pure(StrType)
//       case expr: IntExpr     => pure(IntType)
//       case expr: DblExpr     => pure(DblType)
//       case expr: BoolExpr    => pure(BoolType)
//       case NullExpr          => pure(NullType)
//     }

//   def refType(ref: RefExpr): Step[Type] =
//     lookupType(ref)

//   def letType(defn: LetExpr): Step[Type] =
//     for {
//       exprTpe <- exprType(defn.expr)
//       defnTpe <- defn.tpe match {
//                    case Some(defnTpe) => assertAssignable(defnTpe, exprTpe).map(_ => defnTpe)
//                    case None          => pure(exprTpe)
//                  }
//       _       <- inspectEnv(_.scopes.head.destructiveSet(RefExpr(defn.name), defnTpe))
//     } yield NullType

//   def appType(expr: AppExpr): Step[Type] =
//     ???

//   def infixType(expr: InfixExpr): Step[Type] =
//     for {
//       a   <- exprType(expr.arg1)
//       b   <- exprType(expr.arg2)
//       ans <- infixType(expr.op, a, b)
//     } yield ans

//   def infixType(op: InfixOp, a: Type, b: Type): Step[Type] = {
//     import InfixOp._

//     // TODO: Replace with lookup in InfixFunc:
//     (op, a, b) match {
//       case (Add, IntType, IntType) => pure(IntType)
//       case (Add, IntType, DblType) => pure(DblType)
//       case (Add, DblType, IntType) => pure(DblType)
//       case (Add, DblType, DblType) => pure(DblType)
//       case (Add, StrType, StrType) => pure(StrType)

//       case (Sub, IntType, IntType) => pure(IntType)
//       case (Sub, DblType, IntType) => pure(DblType)
//       case (Sub, IntType, DblType) => pure(DblType)
//       case (Sub, DblType, DblType) => pure(DblType)

//       case (Mul, IntType, IntType) => pure(IntType)
//       case (Mul, DblType, IntType) => pure(DblType)
//       case (Mul, IntType, DblType) => pure(DblType)
//       case (Mul, DblType, DblType) => pure(DblType)

//       case (Div, IntType, IntType) => pure(IntType)
//       case (Div, DblType, IntType) => pure(DblType)
//       case (Div, IntType, DblType) => pure(DblType)
//       case (Div, DblType, DblType) => pure(DblType)

//       case (And, BoolType, BoolType) => pure(BoolType)
//       case (Or,  BoolType, BoolType) => pure(BoolType)

//       case (Eq,  a, b) if a == b => pure(BoolType)
//       case (Ne,  a, b) if a == b => pure(BoolType)

//       case (Gt,  IntType, IntType) => pure(BoolType)
//       case (Gt,  IntType, DblType) => pure(BoolType)
//       case (Gt,  DblType, IntType) => pure(BoolType)
//       case (Gt,  DblType, DblType) => pure(BoolType)
//       case (Gt,  StrType,  StrType)  => pure(BoolType)
//       case (Gt,  BoolType, BoolType) => pure(BoolType)

//       case (Lt,  IntType, IntType) => pure(BoolType)
//       case (Lt,  IntType, DblType) => pure(BoolType)
//       case (Lt,  DblType, IntType) => pure(BoolType)
//       case (Lt,  DblType, DblType) => pure(BoolType)
//       case (Lt,  StrType,  StrType)  => pure(BoolType)
//       case (Lt,  BoolType, BoolType) => pure(BoolType)

//       case (Gte, IntType, IntType) => pure(BoolType)
//       case (Gte, IntType, DblType) => pure(BoolType)
//       case (Gte, DblType, IntType) => pure(BoolType)
//       case (Gte, DblType, DblType) => pure(BoolType)
//       case (Gte, StrType,  StrType)  => pure(BoolType)
//       case (Gte, BoolType, BoolType) => pure(BoolType)

//       case (Lte, IntType, IntType) => pure(BoolType)
//       case (Lte, IntType, DblType) => pure(BoolType)
//       case (Lte, DblType, IntType) => pure(BoolType)
//       case (Lte, DblType, DblType) => pure(BoolType)
//       case (Lte, StrType,  StrType)  => pure(BoolType)
//       case (Lte, BoolType, BoolType) => pure(BoolType)

//       case _ => fail(InfixNotDefined(op, a, b))
//     }
//   }

//   def prefixType(expr: PrefixExpr): Step[Type] =
//     for {
//       a   <- exprType(expr.arg)
//       ans <- prefixType(expr.op, a)
//     } yield ans

//   def prefixType(op: PrefixOp, a: Type): Step[Type] = {
//     import PrefixOp._

//     // TODO: Replace with lookup in PrefixFunc:
//     (op, a) match {
//       case (Not, BoolType) => pure(BoolType)

//       case (Pos, IntType) => pure(IntType)
//       case (Pos, DblType) => pure(DblType)

//       case (Neg, IntType) => pure(IntType)
//       case (Neg, DblType) => pure(DblType)

//       case _ => fail(PrefixNotDefined(op, a))
//     }
//   }

//   def funcType(func: FuncExpr): Step[Type] =
//     for {
//       argTypes <- func.args.traverse {
//                     case FuncArg(name, Some(tpe)) => assignType(RefExpr(name), tpe)
//                     case FuncArg(name, None)      => generateType(RefExpr(name))
//                   }
//       exprType <- exprType(func.body)
//       resType  <- func.resultType match {
//                     case Some(tpe) => assertAssignable(tpe, exprType).map(_ => tpe)
//                     case _         => pure(exprType)
//                   }
//     } yield FuncType(argTypes, resType)

//   def blockType(block: BlockExpr): Step[Type] =
//     pushScope {
//       for {
//         // Iterate, altering typer state but not collecting types:
//         _    <- block.stmts.traverse(exprType).map(_ => ())
//         ans  <- exprType(block.expr)
//       } yield ans
//     }

//   // def selectType(expr: SelectExpr): Step[Type] =
//   //   ???

//   def condType(expr: CondExpr): Step[Type] =
//     for {
//       a   <- exprType(expr.test)
//       _   <- assertAssignable(a, BoolType)
//       b   <- exprType(expr.trueArm)
//       c   <- exprType(expr.falseArm)
//       ans <- unify(b, c)
//     } yield ans

//   def castType(cast: CastExpr): Step[Type] =
//     for {
//       exprTpe <- exprType(cast.expr)
//       _       <- assertAssignable(cast.asType, exprTpe)
//     } yield cast.asType

//   // def objType(obj: ObjExpr): Step[Type] =
//   //   obj.fields
//   //     .traverse { case (n, e) => exprType(e).map(t => Map(n -> t)) }
//   //     .map(ts => Obj(ts.combineAll.toList))

//   // def arrType(arr: ArrExpr): Step[Type] =
//   //   arr.items
//   //     .traverse(exprType)
//   //     .map(_.combineAll)

//   // Apping types to initial native environments:

//   def typeEnv(env: Env): TypeEnv = {
//     TypeEnv(env.scopes.map(typeScope))
//   }

//   def typeScope(scope: Scope): TypeScope =
//     TypeScope(scope.bindings.map { case (n, v) => (RefExpr(n), TypeChecker.valueType(v)) }, Set())

//   def valueType(value: Value): Type =
//     value match {
//       // case ObjVal(fields) => Obj(fields.map { case (n, v) => (n, valueType(v)) })
//       // case ArrVal(items)  => Arr(items.foldLeft(bottom)((t, v) => union(t, valueType(v))))
//       case _: StrVal      => StrType
//       case _: IntVal      => IntType
//       case _: DblVal      => DblType
//       case _: BoolVal     => BoolType
//       case NullVal        => NullType
//       case Closure(f, _)  => ???
//       // case Native(t, _)   => t
//     }

//   // Underlying step constructors:

//   def pureEither[A](either: Either[TypeError, A]): Step[A] =
//     EitherT(State[TypeEnv, Either[TypeError, A]](types => (types, either)))

//   def pure[A](value: A): Step[A] =
//     pureEither(Right(value))

//   def fail[A](error: TypeError): Step[A] =
//     EitherT(State[TypeEnv, Either[TypeError, A]](env => (env, Left(error))))

//   def assertAssignable(to: Type, from: Type): Step[Unit] =
//     (to, from) match {
//       case (a, b) if a == b   => pure(())
//       case (DblType, IntType) => pure(())
//       case (a, b)             => fail(TypeMismatch(a, b))
//     }

//   def unify(a: Type, b: Type): Step[Type] =
//     (a, b) match {
//       case (a, b) if a == b   => pure(a)
//       case (DblType, IntType) => pure(DblType)
//       case (IntType, DblType) => pure(DblType)
//       case (a, b)             => fail(TypeMismatch(a, b))
//     }

//   def inspectEnv[A](func: TypeEnv => A): Step[A] =
//     EitherT(State[TypeEnv, Either[TypeError, A]](env => (env, Right(func(env)))))

//   def currentEnv: Step[TypeEnv] =
//     EitherT(State[TypeEnv, Either[TypeError, TypeEnv]](env => (env, Right(env))))

//   def inspectEnvEither[A](func: TypeEnv => Either[TypeError, A]): Step[A] =
//     EitherT(State[TypeEnv, Either[TypeError, A]](env => (env, func(env))))

//   def modifyEnv(f: TypeEnv => TypeEnv): Step[Unit] =
//     EitherT(State[TypeEnv, Either[TypeError, Unit]](env => (f(env), Right(()))))

//   def lookupType(expr: Expr): Step[Type] =
//     currentEnv.flatMap { env =>
//       env.get(expr) match {
//         case Some(tpe) => pure(tpe)
//         case None      => generateType(expr)
//       }
//     }

//   def assignType(expr: Expr, tpe: Type): Step[Type] =
//     modifyEnv(env => env.set(expr, tpe)).map(_ => tpe)


//   private var _nextTypeVar = 0
//   def generateType(expr: Expr): Step[Type] = {
//     _nextTypeVar = _nextTypeVar + 1
//     val tpe = TypeVar(s"v${_nextTypeVar}")
//     modifyEnv(env  => env.set(expr, tpe)).map(_ => tpe)
//   }

//   def pushScope[A](body: Step[A]): Step[A] =
//     for {
//       _   <- modifyEnv(_.push)
//       ans <- body
//       _   <- modifyEnv(_.pop)
//     } yield ans
// }