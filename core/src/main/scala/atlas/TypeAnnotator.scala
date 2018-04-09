package atlas

import cats.data.{EitherT, State}
import cats.implicits._

object TypeAnnotator {
  type Chain = ScopeChain[String, TypeVar]
  type Env   = (Int, Chain)

  type Step[A] = EitherT[State[Env, ?], TypeError, A]

  def apply(expr: Expr): Either[TypeError, TExpr] =
    doExpr(expr).value.runA((0, ScopeChain.create)).value

  def doExpr(expr: Expr): Step[TExpr] =
    expr match {
      case expr: RefExpr    => doRef(expr)
      case expr: AppExpr    => doApp(expr)
      case expr: InfixExpr  => doInfix(expr)
      case expr: PrefixExpr => doPrefix(expr)
      case expr: FuncExpr   => doFunc(expr)
      case expr: BlockExpr  => doBlock(expr)
      case expr: SelectExpr => doSelect(expr)
      case expr: CondExpr   => doCond(expr)
      case expr: CastExpr   => doCast(expr)
      case expr: ParenExpr  => doParen(expr)
      case expr: ObjExpr    => doObj(expr)
      case expr: ArrExpr    => doArr(expr)
      case expr: StrExpr    => doStr(expr)
      case expr: IntExpr    => doInt(expr)
      case expr: DblExpr    => doDbl(expr)
      case expr: BoolExpr   => doBool(expr)
      case NullExpr         => doNull
    }

  def doRef(ref: RefExpr): Step[TExpr] =
    for {
      tpe <- get(ref.id)
    } yield TRefExpr(tpe, ref.id)

  def doApp(app: AppExpr): Step[TExpr] =
    for {
      tpe  <- gen
      func <- doExpr(app.func)
      args <- app.args.traverse(doExpr)
    } yield TAppExpr(tpe, func, args)

  def doInfix(infix: InfixExpr): Step[TExpr] =
    for {
      tpe  <- gen
      arg1 <- doExpr(infix.arg1)
      arg2 <- doExpr(infix.arg2)
      op    = infix.op
    } yield TInfixExpr(tpe, op, arg1, arg2)

  def doPrefix(prefix: PrefixExpr): Step[TExpr] =
    for {
      tpe <- gen
      arg <- doExpr(prefix.arg)
      op   = prefix.op
    } yield TPrefixExpr(tpe, op, arg)

  def doFunc(func: FuncExpr): Step[TExpr] =
    pushScope {
      for {
        tpe    <- gen
        args   <- func.args.traverse(doFuncArg)
        body   <- doExpr(func.body)
        resType = func.resultType
      } yield TFuncExpr(tpe, args, resType, body)
    }

  def doFuncArg(arg: FuncArg): Step[TFuncArg] =
    for {
      tpe    <- gen
      _      <- set(arg.argName, tpe)
    } yield TFuncArg(tpe, arg.argName, arg.argType)

  def doBlock(block: BlockExpr): Step[TExpr] =
    pushScope {
      for {
        tpe    <- gen
        stmts  <- doStmts(block.stmts)
        expr   <- doExpr(block.expr)
      } yield TBlockExpr(tpe, stmts, expr)
    }

  def doStmts(stmts: List[Stmt]): Step[List[TStmt]] = {
    def preallocate(stmt: Stmt): Step[Unit] =
      stmt match {
        case let: LetStmt   => gen.flatMap(set(let.varName, _))
        case expr: ExprStmt => pure(())
      }

    for {
      _     <- stmts.traverse(preallocate)
      stmts <- stmts.traverse(doStmt)
    } yield stmts
  }

  def doStmt(stmt: Stmt): Step[TStmt] =
    stmt match {
      case let: LetStmt =>
        for {
          tpe    <- get(let.varName)
          expr   <- doExpr(let.expr)
          varName = let.varName
          varType = let.varType
        } yield TLetStmt(tpe, varName, varType, expr)

      case stmt: ExprStmt =>
        for {
          tpe  <- gen
          expr <- doExpr(stmt.expr)
        } yield TExprStmt(tpe, expr)
    }

  def doSelect(select: SelectExpr): Step[TExpr] =
    for {
      tpe  <- gen
      expr <- doExpr(select.expr)
      field = select.field
    } yield TSelectExpr(tpe, expr, field)

  def doParen(select: ParenExpr): Step[TExpr] =
    for {
      tpe  <- gen
      expr <- doExpr(select.expr)
    } yield TParenExpr(tpe, expr)

  def doCond(cond: CondExpr): Step[TExpr] =
    for {
      tpe      <- gen
      test     <- doExpr(cond.test)
      trueArm  <- doExpr(cond.trueArm)
      falseArm <- doExpr(cond.falseArm)
    } yield TCondExpr(tpe, test, trueArm, falseArm)

  def doCast(cast: CastExpr): Step[TExpr] =
    for {
      tpe   <- gen
      expr  <- doExpr(cast.expr)
      asType = cast.asType
    } yield TCastExpr(tpe, expr, asType)

  def doObj(obj: ObjExpr): Step[TExpr] =
    for {
      tpe    <- gen
      fields <- obj.fields.traverse { case (name, expr) => doExpr(expr).map(expr => (name, expr)) }
    } yield TObjExpr(tpe, fields)

  def doArr(arr: ArrExpr): Step[TExpr] =
    for {
      tpe   <- gen
      exprs <- arr.exprs.traverse(doExpr)
    } yield TArrExpr(tpe, exprs)

  def doStr(str: StrExpr): Step[TExpr] =
    gen.map(tpe => TStrExpr(tpe, str.value))

  def doInt(int: IntExpr): Step[TExpr] =
    gen.map(tpe => TIntExpr(tpe, int.value))

  def doDbl(dbl: DblExpr): Step[TExpr] =
    gen.map(tpe => TDblExpr(tpe, dbl.value))

  def doBool(bool: BoolExpr): Step[TExpr] =
    gen.map(tpe => TBoolExpr(tpe, bool.value))

  def doNull: Step[TExpr] =
    gen.map(tpe => TNullExpr(tpe))

  def pure[A](value: A): Step[A] =
    EitherT(State.pure(Right(value)))

  def fail[A](error: TypeError): Step[A] =
    EitherT(State.pure(Left(error)))

  def getNextId: Step[Int] =
    EitherT(State[Env, Either[TypeError, Int]] { case (nextId, chain) => ((nextId, chain), Right(nextId)) })

  def incNextId: Step[Unit] =
    EitherT(State[Env, Either[TypeError, Unit]] { case (nextId, chain) => ((nextId + 1, chain), Right(())) })

  def getChain: Step[Chain] =
    EitherT(State[Env, Either[TypeError, Chain]] { case (nextId, chain) => ((nextId, chain), Right(chain)) })

  def modifyChain(func: Chain => Chain): Step[Unit] =
    EitherT(State[Env, Either[TypeError, Unit]] { case (nextId, chain) => ((nextId, func(chain)), Right(())) })

  def gen: Step[TypeVar] =
    for {
      nextId <- getNextId
      _      <- incNextId
    } yield TypeVar(nextId)

  def get(id: String): Step[TypeVar] =
    for {
      chain <- getChain
      tpe   <- chain.get(id) match {
                 case Some(tpe) => pure(tpe)
                 case None      => fail(TypeError.variableNotFound(id))
               }
    } yield tpe

  def set(id: String, tpe: TypeVar): Step[Unit] =
    modifyChain(chain => chain.set(id, tpe))

  def replaceChain[A](chain: Chain)(body: Step[A]): Step[A] =
    for {
      chain0 <- getChain
      _      <- modifyChain(_ => chain)
      ans    <- body
      _      <- modifyChain(_ => chain0)
    } yield ans

  def pushScope[A](body: Step[A]): Step[A] =
    for {
      _   <- modifyChain(_.push)
      ans <- body
      _   <- modifyChain(_.pop)
    } yield ans
}
