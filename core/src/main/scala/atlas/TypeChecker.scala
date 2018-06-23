// package atlas

// import cats.MonadError
// import cats.data.StateT
// import cats.instances.either._
// import cats.instances.list._
// import cats.syntax.all._

// object TypeEnv {
//   def create: TypeEnv =
//     ScopeChain.create
// }

// object TypeChecker {
//   type Out[A] = Either[TypeError, A]

//   type Step[A] = TypeStep[Out, A]

//   def check(expr: ExprStx, env: TypeEnv = TypeEnv.create): Out[Type] =
//     checkExprStx(expr).runA((0, env))

//   def checkExprStx(expr: ExprStx): Step[Type] =
//     expr match {
//       case expr: RefExprStx    => checkRef(expr)
//       case expr: AppExprStx    => checkApp(expr)
//       case expr: InfixExprStx  => checkInfix(expr)
//       case expr: PrefixExprStx => checkPrefix(expr)
//       case expr: FuncExprStx   => checkFunc(expr)
//       case expr: BlockExprStx  => checkBlock(expr)
//       case expr: SelectExprStx => checkSelect(expr)
//       case expr: CondExprStx   => checkCond(expr)
//       case expr: CastExprStx   => checkCast(expr)
//       case ParenExprStx(expr)  => checkExprStx(expr)
//       case expr: ObjExprStx    => checkObj(expr)
//       case expr: ArrExprStx    => checkArr(expr)
//       case StrExprStx(value)   => pure(StrType)
//       case IntExprStx(value)   => pure(IntType)
//       case DblExprStx(value)   => pure(DblType)
//       case BoolExprStx(value)  => pure(BoolType)
//       case NullExprStx         => pure(NullType)
//     }

//   def checkRef(ref: RefExprStx): Step[Type] =
//     getType(ref.name)

//   def checkApp(apply: AppExprStx): Step[Type] =
//     ???

//   def checkInfix(infix: InfixExprStx): Step[Type] =
//     ???

//   def checkPrefix(prefix: PrefixExprStx): Step[Type] =
//     ???

//   def checkFunc(func: FuncExprStx): Step[Type] =
//     pushScope {
//       def checkFuncArg(arg: FuncArgStx): Step[Unit] =
//         arg.tpe match {
//           case Some(tpe) => setType(arg.name, tpe)
//           case None      => genType.flatMap(setType(arg.name, _))
//         }

//       for {
//         args <- func.args.traverse(checkFuncArg)
//         expr <- checkExprStx(func.body)
//         ans  <- func.retType.fold(pure(expr))(assertAssignable(_, expr))
//       } yield ans
//     }

//   def checkBlock(block: BlockExprStx): Step[Type] =
//     for {
//       _   <- checkStmts(block.stmts)
//       ans <- checkExprStx(block.expr)
//     } yield ans

//   def checkStmts(stmts: List[Stmt]): Step[Unit] =
//     stmts.traverse(checkStmt).map(_ => ())

//   def checkStmt(stmt: Stmt): Step[Unit] =
//     stmt match {
//       case stmt: ExprStxStmt    => checkExprStx(stmt.expr).map(_ => ())
//       case stmt: LetStmt     => checkLetStmt(stmt)
//       case stmt: TypeStmt => checkTypeStmt(stmt)
//     }

//   def checkExprStxStmt(stmt: ExprStxStmt): Step[Unit] =
//     checkExprStx(stmt.expr).map(_ => ())

//   def checkLetStmt(stmt: LetStmt): Step[Unit] =
//     for {
//       expr <- checkExprStx(stmt.expr)
//       tpe  <- stmt.tpe.fold(pure(expr))(assertAssignable(_, expr))
//       _    <- setType(stmt.name, tpe)
//     } yield ()

//   def checkTypeStmt(stmt: TypeStmt): Step[Unit] =
//     aliasType(stmt.typeName, stmt.asType)

//   def checkSelect(select: SelectExprStx): Step[Type] =
//     ???

//   def checkCond(cond: CondExprStx): Step[Type] =
//     for {
//       test     <- checkExprStx(cond.test)
//       _        <- assertAssignable(BoolType, test)
//       trueArm  <- checkExprStx(cond.trueArm)
//       falseArm <- checkExprStx(cond.falseArm)
//     } yield Type.union(trueArm, falseArm)

//   def checkCast(cast: CastExprStx): Step[Type] =
//     for {
//       expr <- checkExprStx(cast.expr)
//       tpe  <- assertAssignable(cast.asType, expr)
//     } yield tpe

//   def checkObj(obj: ObjExprStx): Step[Type] =
//     ???

//   def checkArr(arr: ArrExprStx): Step[Type] =
//     arr.exprs.traverse(checkExprStx).map(types => ArrType(Type.unionAll(types)))

//   // Resolving, unifying, and checking types ----

//   /** Check we can assign to type `a` with a value of type `b`. */
//   def assertAssignable(a: Type, b: Type): Step[Type] =
//     for {
//       a   <- resolveType(a)
//       b   <- resolveType(b)
//       ans <- if(Type.isAssignable(a, b)) {
//                pure(a)
//              } else {
//                fail(TypeError.typeMismatch(a, b))
//              }
//     } yield ans

//   /** Unify `a` and `b`, setting type variables as necessary. */
//   def unify(a: Type, b: Type): Step[Type] =
//     (a, b) match {
//       case (a: RefType, b: RefType) => resolveType(b).flatMap(unify(a, _))
//       case (a: RefType, b         ) => aliasType(a.name, b).map(_ => b)
//       case (a         , b: RefType) => aliasType(b.name, a).map(_ => a)
//       case (a         , b         ) => assertAssignable(a, b).map(_ => a)
//     }

//   def resolveType(tpe: Type): Step[Type] =
//     tpe match {
//       case ref: RefType =>
//         inspectEnv(env => env.get(s"type:${ref.name}").pure[Out])
//           .flatMap[Type, (Int, TypeEnv)] {
//             case Some(tpe) => resolveType(tpe)
//             case None      => fail(TypeError.typeNotFound(ref.name))
//           }

//       case FuncType(args, res) =>
//         for {
//           args <- args.traverse(resolveType)
//           res  <- resolveType(res)
//         } yield FuncType(args, res)

//       case UnionType(types) =>
//         for {
//           types <- types.toList.traverse(resolveType)
//         } yield UnionType(types.toSet)

//       case tpe: Type =>
//         pure(tpe)
//     }

//   def aliasType(typeName: String, tpe: Type): Step[Unit] =
//     inspectEnv(env => env.destructiveSet(s"type:$typeName", tpe).pure[Out])

//   def genType: Step[RefType] =
//     nextTypeId(id => RefType(s"!$id").pure[Out])

//   def getType(name: String): Step[Type] =
//     inspectEnv(env => env.get(s"var:$name").pure[Out])
//       .flatMap[Type, (Int, TypeEnv)] {
//         case Some(tpe) => pure(tpe)
//         case None      => fail(TypeError.variableNotFound(name))
//       }
//       .flatMap(resolveType)

//   def setType(name: String, value: Type): Step[Unit] =
//     inspectEnv(env => env.destructiveSet(s"var:$name", value).pure[Out])

//   // Environment primitives ---------------------

//   def pushScope[A](body: Step[A]): Step[A] =
//     for {
//       _   <- updateEnv(_.push.pure[Out])
//       ans <- body
//       _   <- updateEnv(_.pop.pure[Out])
//     } yield ans

//   def inspectEnv[A](func: TypeEnv => Out[A]): Step[A] =
//     StateT.inspectF { case (nextId, env) => func(env) }

//   def updateEnv(func: TypeEnv => Out[TypeEnv]): Step[Unit] =
//     StateT.modifyF { case (nextId, env) => func(env).map(env => (nextId, env)) }

//   def nextTypeId[A](func: Int => Out[A]): Step[A] =
//     StateT.apply { case (nextId, env) => func(nextId).map(a => ((nextId + 1, env), a)) }

//   // Success/failure primitives -----------------

//   def pure[A](value: A): Step[A] =
//     value.pure[Step]

//   def fail[A](error: TypeError): Step[A] =
//     error.raiseError[Step, A]
// }