// package atlas

// import cats.MonadError
// import cats.data.StateT
// import cats.instances.either._
// import cats.instances.list._
// import cats.syntax.all._

// object TypeChecker {
//   type Out[A] = Either[TypeError, A]

//   type Step[A] = TypeStep[Out, A]

//   def check(expr: Expr, env: TypeEnv = TypeEnv.create): Out[Type] =
//     checkExpr(expr).runA((0, env))

//   def checkExpr(expr: Expr): Step[Type] =
//     expr match {
//       case expr: RefExpr    => checkRef(expr)
//       case expr: AppExpr    => checkApp(expr)
//       case expr: InfixExpr  => checkInfix(expr)
//       case expr: PrefixExpr => checkPrefix(expr)
//       case expr: FuncExpr   => checkFunc(expr)
//       case expr: BlockExpr  => checkBlock(expr)
//       case expr: SelectExpr => checkSelect(expr)
//       case expr: CondExpr   => checkCond(expr)
//       case expr: CastExpr   => checkCast(expr)
//       case ParenExpr(expr)  => checkExpr(expr)
//       case expr: ObjExpr    => checkObj(expr)
//       case expr: ArrExpr    => checkArr(expr)
//       case StrExpr(value)   => pure(StrType)
//       case IntExpr(value)   => pure(IntType)
//       case DblExpr(value)   => pure(DblType)
//       case BoolExpr(value)  => pure(BoolType)
//       case NullExpr         => pure(NullType)
//     }

//   def checkRef(ref: RefExpr): Step[Type] =
//     getType(ref.id)

//   def checkApp(apply: AppExpr): Step[Type] =
//     ???

//   def checkInfix(infix: InfixExpr): Step[Type] =
//     ???

//   def checkPrefix(prefix: PrefixExpr): Step[Type] =
//     ???

//   def checkFunc(func: FuncExpr): Step[Type] =
//     pushScope {
//       def checkFuncArg(arg: FuncArg): Step[Unit] =
//         arg.argType match {
//           case Some(tpe) => setType(arg.argName, tpe)
//           case None      => genType.flatMap(setType(arg.argName, _))
//         }

//       for {
//         args <- func.args.traverse(checkFuncArg)
//         expr <- checkExpr(func.body)
//         ans  <- func.retType.fold(pure(expr))(assertAssignable(_, expr))
//       } yield ans
//     }

//   def checkBlock(block: BlockExpr): Step[Type] =
//     for {
//       _   <- checkStmts(block.stmts)
//       ans <- checkExpr(block.expr)
//     } yield ans

//   def checkStmts(stmts: List[Stmt]): Step[Unit] =
//     stmts.traverse(checkStmt).map(_ => ())

//   def checkStmt(stmt: Stmt): Step[Unit] =
//     stmt match {
//       case stmt: ExprStmt    => checkExpr(stmt.expr).map(_ => ())
//       case stmt: LetStmt     => checkLetStmt(stmt)
//       case stmt: LetTypeStmt => checkLetTypeStmt(stmt)
//     }

//   def checkExprStmt(stmt: ExprStmt): Step[Unit] =
//     checkExpr(stmt.expr).map(_ => ())

//   def checkLetStmt(stmt: LetStmt): Step[Unit] =
//     for {
//       expr <- checkExpr(stmt.expr)
//       tpe  <- stmt.varType.fold(pure(expr))(assertAssignable(_, expr))
//       _    <- setType(stmt.varName, tpe)
//     } yield ()

//   def checkLetTypeStmt(stmt: LetTypeStmt): Step[Unit] =
//     aliasType(stmt.typeName, stmt.asType)

//   def checkSelect(select: SelectExpr): Step[Type] =
//     ???

//   def checkCond(cond: CondExpr): Step[Type] =
//     for {
//       test     <- checkExpr(cond.test)
//       _        <- assertAssignable(BoolType, test)
//       trueArm  <- checkExpr(cond.trueArm)
//       falseArm <- checkExpr(cond.falseArm)
//     } yield Type.union(trueArm, falseArm)

//   def checkCast(cast: CastExpr): Step[Type] =
//     for {
//       expr <- checkExpr(cast.expr)
//       tpe  <- assertAssignable(cast.asType, expr)
//     } yield tpe

//   def checkObj(obj: ObjExpr): Step[Type] =
//     ???

//   def checkArr(arr: ArrExpr): Step[Type] =
//     arr.exprs.traverse(checkExpr).map(types => ArrType(Type.unionAll(types)))

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
//       case (a: TypeRef, b: TypeRef) => resolveType(b).flatMap(unify(a, _))
//       case (a: TypeRef, b         ) => aliasType(a.name, b).map(_ => b)
//       case (a         , b: TypeRef) => aliasType(b.name, a).map(_ => a)
//       case (a         , b         ) => assertAssignable(a, b).map(_ => a)
//     }

//   def resolveType(tpe: Type): Step[Type] =
//     tpe match {
//       case ref: TypeRef =>
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

//   def genType: Step[TypeRef] =
//     nextTypeId(id => TypeRef(s"!$id").pure[Out])

//   def getType(varName: String): Step[Type] =
//     inspectEnv(env => env.get(s"var:$varName").pure[Out])
//       .flatMap[Type, (Int, TypeEnv)] {
//         case Some(tpe) => pure(tpe)
//         case None      => fail(TypeError.variableNotFound(varName))
//       }
//       .flatMap(resolveType)

//   def setType(varName: String, value: Type): Step[Unit] =
//     inspectEnv(env => env.destructiveSet(s"var:$varName", value).pure[Out])

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