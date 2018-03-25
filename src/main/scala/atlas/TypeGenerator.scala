package atlas

import atlas.syntax._
import cats.data.{EitherT, State}
import cats.implicits._

case class Constraint(lhs: Type, rhs: Type) {
  override def toString = s"$lhs === $rhs"
}

object Constraint {
  implicit val ordering: Ordering[Constraint] =
    Ordering.by[Constraint, (Type, Type)](c => (c.lhs, c.rhs))
}

object TypeGenerator {
  type Step[A] = Either[TypeError, A]

  def apply(expr: TExpr): Either[TypeError, List[Constraint]] =
    Time("generator")(doExpr(expr).map(_.toList.sorted))

  def doExpr(expr: TExpr): Step[Set[Constraint]] =
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
      case expr: TArrExpr    => doArr(expr)
      case expr: TStrExpr    => doStr(expr)
      case expr: TIntExpr    => doInt(expr)
      case expr: TDblExpr    => doDbl(expr)
      case expr: TBoolExpr   => doBool(expr)
      case expr: TNullExpr   => doNull(expr)
    }

  def doRef(ref: TRefExpr): Step[Set[Constraint]] =
    assign()

  def doLet(let: TLetExpr): Step[Set[Constraint]] =
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

  def doApp(app: TAppExpr): Step[Set[Constraint]] =
    all(
      doExpr(app.func),
      app.args.traverse(doExpr).map(_.combineAll),
      assign(app.func.tpe === FuncType(app.args.map(_.tpe), app.tpe))
    )

  def doInfix(infix: TInfixExpr): Step[Set[Constraint]] =
    all(
      doExpr(infix.arg1),
      doExpr(infix.arg2),
      assign(FuncType(List(infix.arg1.tpe, infix.arg2.tpe), infix.tpe) === infixType(infix.op))
    )

  def infixType(op: InfixOp): Type =
    // TODO: Make infix operators polymorphic!
    // TODO: Move infix operator type definitions out to external file
    op match {
      case InfixOp.Add => FuncType(List(IntType, IntType), IntType) |
                          FuncType(List(DblType, DblType), DblType)
      case InfixOp.Sub => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Mul => FuncType(List(IntType, IntType), IntType)
      case InfixOp.Div => FuncType(List(IntType, IntType), IntType)
      case InfixOp.And => FuncType(List(BoolType, BoolType), BoolType)
      case InfixOp.Or  => FuncType(List(BoolType, BoolType), BoolType)
      case InfixOp.Eq  => FuncType(List(IntType, IntType), BoolType)
      case InfixOp.Ne  => FuncType(List(IntType, IntType), BoolType)
      case InfixOp.Gt  => FuncType(List(IntType, IntType), BoolType)
      case InfixOp.Lt  => FuncType(List(IntType, IntType), BoolType)
      case InfixOp.Gte => FuncType(List(IntType, IntType), BoolType)
      case InfixOp.Lte => FuncType(List(IntType, IntType), BoolType)
    }

  def doPrefix(prefix: TPrefixExpr): Step[Set[Constraint]] =
    all(
      doExpr(prefix.arg),
      assign(FuncType(List(prefix.arg.tpe), prefix.tpe) === prefixType(prefix.op))
    )

  def prefixType(op: PrefixOp): Type =
    // TODO: Make prefix operators polymorphic!
    // TODO: Move prefix operator type definitions out to external file
    op match {
      case PrefixOp.Not => FuncType(List(BoolType), BoolType)
      case PrefixOp.Pos => FuncType(List(IntType), IntType)
      case PrefixOp.Neg => FuncType(List(IntType), IntType)
    }

  def doFunc(func: TFuncExpr): Step[Set[Constraint]] =
    all(
      doExpr(func.body),
      func.args.traverse(doFuncArg).map(_.combineAll),
      func.resultType match {
        case Some(resultType) =>
          assign(
            func.tpe === resultType,
            func.tpe === FuncType(func.args.map(_.tpe), func.body.tpe)
          )

        case None =>
          assign(
            func.tpe === FuncType(func.args.map(_.tpe), func.body.tpe)
          )
      }
    )

  def doFuncArg(arg: TFuncArg): Step[Set[Constraint]] =
    arg.argType match {
      case Some(argType) =>
        assign(
          arg.tpe === argType
        )

      case None =>
        assign()
    }

  def doBlock(block: TBlockExpr): Step[Set[Constraint]] =
    all(
      block.stmts.traverse(doExpr).map(_.combineAll),
      doExpr(block.expr),
      assign(
        block.tpe === block.expr.tpe
      )
    )

  // def doSelect(select: SelectExpr): Step[Set[Constraint]] =
  //   ???

  def doCond(cond: TCondExpr): Step[Set[Constraint]] =
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

  def doCast(cast: TCastExpr): Step[Set[Constraint]] =
    all(
      doExpr(cast.expr),
      assign(
        cast.tpe === cast.expr.tpe,
        cast.tpe === cast.asType
      )
    )

  // def doObj(obj: TObjExpr): Step[Set[Constraint]] =
  //   ???

  def doArr(arr: TArrExpr): Step[Set[Constraint]] =
    if(arr.exprs.isEmpty) {
      assign(arr.tpe === AnyType)
    } else {
      all(
        arr.exprs.traverse(doExpr).map(_.combineAll),
        pure(arr.exprs.map(expr => arr.tpe === ArrType(expr.tpe)).toSet)
      )
    }

  def doStr(str: TStrExpr): Step[Set[Constraint]] =
    assign(str.tpe === StrType)

  def doInt(int: TIntExpr): Step[Set[Constraint]] =
    assign(int.tpe === IntType)

  def doDbl(dbl: TDblExpr): Step[Set[Constraint]] =
    assign(dbl.tpe === DblType)

  def doBool(bool: TBoolExpr): Step[Set[Constraint]] =
    assign(bool.tpe === BoolType)

  def doNull(expr: TNullExpr): Step[Set[Constraint]] =
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

  def assign(constraints: Constraint *): Step[Set[Constraint]] =
    Right(constraints.toSet)

  def all(steps: Step[Set[Constraint]] *): Step[Set[Constraint]] =
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