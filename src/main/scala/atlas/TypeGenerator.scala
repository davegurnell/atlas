package atlas

import atlas.syntax._
import cats.data.{EitherT, State}
import cats.implicits._

case class Constraint(lhs: Type, rhs: Type)

object TypeGenerator {
  type Constraints = List[Constraint]
  type Step[A]     = Either[TypeError, A]

  def apply(expr: TExpr): Either[TypeError, Constraints] =
    doExpr(expr)

  def doExpr(expr: TExpr): Step[Constraints] =
    expr match {
      case expr: TRefExpr    => doRef(expr)
      case expr: TLetExpr    => doLet(expr)
      case expr: TAppExpr    => doApp(expr)
      case expr: TInfixExpr  => doInfix(expr)
      case expr: TPrefixExpr => doPrefix(expr)
      case expr: TFuncExpr   => doFunc(expr)
      case expr: TBlockExpr  => doBlock(expr)
      // case expr: TSelectExpr => doSelect(expr)
      case expr: TCondExpr   => doCond(expr)
      case expr: TCastExpr   => doCast(expr)
      // case expr: TObjExpr    => doObj(expr)
      // case expr: TArrExpr    => doArr(expr)
      case expr: TStrExpr    => doStr(expr)
      case expr: TIntExpr    => doInt(expr)
      case expr: TDblExpr    => doDbl(expr)
      case expr: TBoolExpr   => doBool(expr)
      case expr: TNullExpr   => doNull(expr)
    }

  def doRef(ref: TRefExpr): Step[Constraints] =
    assign()

  def doLet(let: TLetExpr): Step[Constraints] =
    all(
      doExpr(let.expr),
      let.varType match {
        case Some(varType) =>
          assign(
            let.tpe === varType,
            let.tpe === let.expr.tpe
          )
        case None =>
          assign(
            let.tpe === let.expr.tpe
          )
      }
    )

  def doApp(app: TAppExpr): Step[Constraints] =
    all(
      doExpr(app.func),
      app.args.traverse(doExpr).map(_.combineAll),
      assign(app.tpe === FuncType(app.args.map(_.tpe), app.func.tpe))
    )

  def doInfix(infix: TInfixExpr): Step[Constraints] = {
    val funcType = infixType(infix.op)
    all(
      doExpr(infix.arg1),
      doExpr(infix.arg2),
      assign(
        infix.arg1.tpe === funcType.argTypes(0),
        infix.arg2.tpe === funcType.argTypes(1),
        infix.tpe      === funcType.returnType
      )
    )
  }

  def infixType(op: InfixOp): FuncType =
    // TODO: Make infix operators polymorphic!
    // TODO: Move infix operator type definitions out to external file
    op match {
      case InfixOp.Add => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Sub => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Mul => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Div => FuncType(List(IntType, IntType), IntType)
      case InfixOp.And => FuncType(List(BoolType, BoolType), BoolType)
      case InfixOp.Or  => FuncType(List(BoolType, BoolType), BoolType)
      case InfixOp.Eq  => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Ne  => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Gt  => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Lt  => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Gte => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Lte => FuncType(List(IntType, IntType), IntType)
    }

  def doPrefix(prefix: TPrefixExpr): Step[Constraints] = {
    val funcType = prefixType(prefix.op)
    all(
      doExpr(prefix.arg),
      assign(
        prefix.arg.tpe === funcType.argTypes(0),
        prefix.tpe     === funcType.returnType
      )
    )
  }

  def prefixType(op: PrefixOp): FuncType =
    // TODO: Make prefix operators polymorphic!
    // TODO: Move prefix operator type definitions out to external file
    op match {
      case PrefixOp.Not => FuncType(List(BoolType), BoolType)
      case PrefixOp.Pos => FuncType(List(IntType), IntType)
      case PrefixOp.Neg => FuncType(List(IntType), IntType)
    }

  def doFunc(func: TFuncExpr): Step[Constraints] =
    all(
      doExpr(func.body),
      func.args.traverse(doFuncArg).map(_.combineAll),
      func.resultType match {
        case Some(resultType) =>
          assign(
            func.tpe === resultType,
            func.tpe === func.body.tpe
          )

        case None =>
          assign(
            func.tpe === func.body.tpe
          )
      }
    )

  def doFuncArg(arg: TFuncArg): Step[Constraints] =
    arg.argType match {
      case Some(argType) =>
        assign(
          arg.tpe === argType
        )

      case None =>
        assign()
    }

  def doBlock(block: TBlockExpr): Step[Constraints] =
    all(
      block.stmts.traverse(doExpr).map(_.combineAll),
      doExpr(block.expr),
      assign(
        block.tpe === block.expr.tpe
      )
    )

  // def doSelect(select: SelectExpr): Step[Constraints] =
  //   ???

  def doCond(cond: TCondExpr): Step[Constraints] =
    all(
      doExpr(cond.test),
      doExpr(cond.trueArm),
      doExpr(cond.falseArm),
      assign(
        cond.test.tpe === BoolType,
        cond.tpe === cond.trueArm.tpe,
        cond.tpe === cond.falseArm.tpe
      )
    )

  def doCast(cast: TCastExpr): Step[Constraints] =
    all(
      doExpr(cast.expr),
      assign(
        cast.tpe === cast.expr.tpe,
        cast.tpe === cast.asType
      )
    )

  // def doObj(obj: ObjExpr): Step[Constraints] =
  //   ???

  // def doArr(arr: ArrExpr): Step[Constraints] =
  //   ???

  def doStr(str: TStrExpr): Step[Constraints] =
    assign(str.tpe === StrType)

  def doInt(int: TIntExpr): Step[Constraints] =
    assign(int.tpe === IntType)

  def doDbl(dbl: TDblExpr): Step[Constraints] =
    assign(dbl.tpe === DblType)

  def doBool(bool: TBoolExpr): Step[Constraints] =
    assign(bool.tpe === BoolType)

  def doNull(expr: TNullExpr): Step[Constraints] =
    assign(expr.tpe === NullType)

  def pure[A](value: A): Step[A] =
    Right(value)

  def fail[A](error: TypeError): Step[A] =
    Left(error)

  // def getNextId: Step[Int] =
  //   EitherT(State[Env, Either[TypeError, Int]] { case (nextId, chain) => ((nextId, chain), Right(nextId)) })

  // def incNextId: Step[Unit] =
  //   EitherT(State[Env, Either[TypeError, Unit]] { case (nextId, chain) => ((nextId + 1, chain), Right(())) })

  // def getChain: Step[Chain] =
  //   EitherT(State[Env, Either[TypeError, Chain]] { case (nextId, chain) => ((nextId, chain), Right(chain)) })

  // def modifyChain(func: Chain => Chain): Step[Unit] =
  //   EitherT(State[Env, Either[TypeError, Unit]] { case (nextId, chain) => ((nextId, func(chain)), Right(())) })

  // def gen: Step[TypeVar] =
  //   for {
  //     nextId <- getNextId
  //     _      <- incNextId
  //   } yield TypeVar(nextId)

  // def get(id: String): Step[TypeVar] =
  //   for {
  //     chain <- getChain
  //     tpe   <- chain.get(id) match {
  //                case Some(tpe) => pure(tpe)
  //                case None      => fail(VariableNotFound(id))
  //              }
  //   } yield tpe

  def assign(constraints: Constraint *): Step[Constraints] =
    Right(constraints.toList)

  def all(steps: Step[Constraints] *): Step[Constraints] =
    steps.toList.sequence.map(_.combineAll)

  // def replaceChain[A](chain: Chain)(body: Step[A]): Step[A] =
  //   for {
  //     chain0 <- getChain
  //     _      <- modifyChain(_ => chain)
  //     ans    <- body
  //     _      <- modifyChain(_ => chain0)
  //   } yield ans

  // def pushScope[A](body: Step[A]): Step[A] =
  //   for {
  //     _   <- modifyChain(_.push)
  //     ans <- body
  //     _   <- modifyChain(_.pop)
  //   } yield ans

  def debug[A](message: String)(value: => A): A = {
    val ans = value
    println(message + ": " + ans)
    ans
  }
}