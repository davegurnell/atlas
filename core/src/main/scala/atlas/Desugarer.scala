package atlas

import cats.MonadError
import cats.data.StateT
import cats.instances.either._
import cats.instances.list._
import cats.syntax.all._

object DesugarEnv {
  def create: DesugarEnv =
    ScopeChain.create
}

object Desugarer {
  type Out[A] = A

  type Step[A] = DesugarStep[Out, A]

  def desugar(expr: ExprStx): Out[Expr] =
    desugarExpr(expr).runA(0)

  def desugarExpr(expr: ExprStx): Step[Expr] =
    expr match {
      case expr: RefExprStx    => desugarRef(expr)
      case expr: AppExprStx    => desugarApp(expr)
      case expr: InfixExprStx  => desugarInfix(expr)
      case expr: PrefixExprStx => desugarPrefix(expr)
      case expr: FuncExprStx   => desugarFunc(expr)
      case expr: BlockExprStx  => desugarBlock(expr)
      case expr: SelectExprStx => desugarSelect(expr)
      case expr: CondExprStx   => desugarCond(expr)
      case expr: CastExprStx   => desugarCast(expr)
      case ParenExprStx(expr)  => desugarExpr(expr)
      case expr: ObjExprStx    => desugarObj(expr)
      case expr: ArrExprStx    => desugarArr(expr)
      case StrExprStx(value)   => pure(StrExpr(value))
      case IntExprStx(value)   => pure(IntExpr(value))
      case DblExprStx(value)   => pure(DblExpr(value))
      case BoolExprStx(value)  => pure(BoolExpr(value))
      case NullExprStx         => pure(NullExpr)
    }

  def desugarRef(ref: RefExprStx): Step[Expr] =
    pure(RefExpr(ref.name))

  def desugarApp(apply: AppExprStx): Step[Expr] =
    for {
      func <- desugarExpr(apply.func)
      args <- apply.args.traverse(desugarExpr)
    } yield AppExpr(func, args)

  def desugarInfix(infix: InfixExprStx): Step[Expr] =
    for {
      arg1 <- desugarExpr(infix.arg1)
      arg2 <- desugarExpr(infix.arg2)
    } yield AppExpr(RefExpr(s"infix:${infix.op.id}"), List(arg1, arg2))

  def desugarPrefix(prefix: PrefixExprStx): Step[Expr] =
    for {
      arg <- desugarExpr(prefix.arg)
    } yield AppExpr(RefExpr(s"prefix:${prefix.op.id}"), List(arg))

  def desugarFunc(func: FuncExprStx): Step[Expr] =
    for {
      args        <- func.args.traverse(desugarFuncArg)
      resType     <- func.resType.fold(genTypeVar)(desugarType)
      typeVars     = func.typeVars.map(TypeVar(_))
      argTypeVars  = args.collect { case FuncArg(_, t: RefType) => t }
      resTypeVar   = Option(resType).collect { case t: RefType => t }
      body        <- desugarExpr(func.body)
    } yield FuncExpr(typeVars ++ argTypeVars ++ resTypeVar, args, resType, body)

  def desugarFuncArg(arg: FuncArgStx): Step[FuncArg] =
    arg.tpe.fold(genTypeVar)(desugarType).map(tpe => FuncArg(arg.name, tpe))

  def desugarBlock(block: BlockExprStx): Step[Expr] =
    for {
      stmts <- desugarStmts(block.stmts)
      expr  <- desugarExpr(block.expr)
    } yield if(stmts.isEmpty) expr else BlockExpr(stmts, expr)

  def desugarSelect(select: SelectExprStx): Step[Expr] =
    for {
      expr <- desugarExpr(select.expr)
    } yield SelectExpr(expr, select.field)

  def desugarCond(cond: CondExprStx): Step[Expr] =
    for {
      test     <- desugarExpr(cond.test)
      trueArm  <- desugarExpr(cond.trueArm)
      falseArm <- desugarExpr(cond.falseArm)
    } yield CondExpr(test, trueArm, falseArm)

  def desugarCast(cast: CastExprStx): Step[Expr] =
    for {
      expr   <- desugarExpr(cast.expr)
      asType <- desugarType(cast.asType)
    } yield CastExpr(expr, asType)

  def desugarObj(obj: ObjExprStx): Step[Expr] =
    for {
      fields <- obj.fields.traverse { case (name, expr) => desugarExpr(expr).map((name, _)) }
    } yield ObjExpr(fields)

  def desugarArr(arr: ArrExprStx): Step[Expr] =
    for {
      exprs <- arr.exprs.traverse(desugarExpr)
    } yield ArrExpr(exprs)

  def desugarStmts(stmts: List[StmtStx]): Step[List[Stmt]] =
    stmts.traverse(desugarStmt)

  def desugarStmt(stmt: StmtStx): Step[Stmt] =
    stmt match {
      case stmt: ExprStmtStx => desugarExprStmt(stmt)
      case stmt: LetStmtStx  => desugarLetStmt(stmt)
      case stmt: TypeStmtStx => desugarTypeStmt(stmt)
    }

  def desugarExprStmt(stmt: ExprStmtStx): Step[Stmt] =
    desugarExpr(stmt.expr).map(ExprStmt)

  def desugarLetStmt(stmt: LetStmtStx): Step[Stmt] =
    for {
      expr <- desugarExpr(stmt.expr)
      tpe  <- stmt.tpe.fold(genTypeVar)(desugarType)
    } yield LetStmt(stmt.name, tpe, expr)

  def desugarTypeStmt(stmt: TypeStmtStx): Step[Stmt] =
    for {
      tpe  <- desugarType(stmt.tpe)
    } yield TypeStmt(stmt.name, tpe)

  def desugarType(tpe: TypeStx): Step[Type] =
    tpe match {
      case tpe: RefTypeStx      => desugarRefType(tpe)
      case tpe: FuncTypeStx     => desugarFuncType(tpe)
      case tpe: UnionTypeStx    => desugarUnionType(tpe)
      case tpe: NullableTypeStx => desugarNullableType(tpe)
      case tpe: ObjTypeStx      => desugarObjType(tpe)
      case tpe: ArrTypeStx      => desugarArrType(tpe)
      case tpe: ParenTypeStx    => desugarParenType(tpe)
      case StrTypeStx           => pure(StrType)
      case IntTypeStx           => pure(IntType)
      case DblTypeStx           => pure(DblType)
      case BoolTypeStx          => pure(BoolType)
      case NullTypeStx          => pure(NullType)
    }

  def desugarRefType(tpe: RefTypeStx): Step[Type] =
    pure(RefType(tpe.name))

  def desugarFuncType(tpe: FuncTypeStx): Step[Type] =
    for {
      args <- tpe.argTypes.traverse(desugarType)
      res  <- desugarType(tpe.resType)
    } yield FuncType(args, res)

  def desugarUnionType(tpe: UnionTypeStx): Step[Type] =
    for {
      a <- desugarType(tpe.a)
      b <- desugarType(tpe.b)
    } yield Type.union(a, b)

  def desugarNullableType(tpe: NullableTypeStx): Step[Type] =
    desugarType(tpe.arg).map(_.?)

  def desugarObjType(tpe: ObjTypeStx): Step[Type] =
    tpe.fields
      .traverse { case (n, t) => desugarType(t).map(t => (n, t)) }
      .map(ObjType)

  def desugarArrType(tpe: ArrTypeStx): Step[Type] =
    desugarType(tpe.arg).map(ArrType)

  def desugarParenType(tpe: ParenTypeStx): Step[Type] =
    desugarType(tpe.tpe)

  // Resolving, unifying, and desugaring types ----

  def genTypeVar: Step[Type] =
    nextTypeId(id => RefType(s"?$id").pure[Out])

  // Environment primitives ---------------------

  def nextTypeId[A](func: Int => Out[A]): Step[A] =
    StateT.apply(nextId => func(nextId).map(a => (nextId + 1, a)))

  // Success/failure primitives -----------------

  def pure[A](value: A): Step[A] =
    value.pure[Step]
}