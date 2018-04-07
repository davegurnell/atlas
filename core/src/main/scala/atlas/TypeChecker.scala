package atlas

import cats.MonadError
import cats.syntax.all._

class TypeChecker[F[_]](implicit val monad: MonadError[F, TypeError]) {
  type Out[A] = F[A]

  type Step[A] = TypeStep[F, A]

  def check(expr: TExpr, env: TypeEnv): F[Type] =
    checkExpr(expr).runA(env)

  def checkExpr(expr: TExpr): Step[Type] =
    expr match {
      case expr: TRefExpr      => checkRef(expr)
      case expr: TAppExpr      => checkApp(expr)
      case expr: TInfixExpr    => checkInfix(expr)
      case expr: TPrefixExpr   => checkPrefix(expr)
      case expr: TFuncExpr     => checkFunc(expr)
      case expr: TBlockExpr    => checkBlock(expr)
      case expr: TSelectExpr   => checkSelect(expr)
      case expr: TCondExpr     => checkCond(expr)
      case expr: TCastExpr     => checkCast(expr)
      case TParenExpr(_, expr) => checkExpr(expr)
      case expr: TObjExpr      => checkObj(expr)
      case expr: TArrExpr      => checkArr(expr)
      case TStrExpr(_, value)  => pure(StrType)
      case TIntExpr(_, value)  => pure(IntType)
      case TDblExpr(_, value)  => pure(DblType)
      case TBoolExpr(_, value) => pure(BoolType)
      case TNullExpr(_)        => pure(NullType)
    }

  def checkRef(ref: TRefExpr): Step[Type] =
    ???

  def checkApp(apply: TAppExpr): Step[Type] =
    ???

  def checkInfix(infix: TInfixExpr): Step[Type] =
    ???

  def checkPrefix(prefix: TPrefixExpr): Step[Type] =
    ???

  def checkFunc(func: TFuncExpr): Step[Type] =
    ???

  def checkBlock(block: TBlockExpr): Step[Type] =
    ???

  def checkStmts(stmts: List[TStmt]): Step[Unit] =
    ???

  def checkStmt(stmt: TStmt): Step[Unit] =
    ???

  def checkLetStmt(stmt: TLetStmt): Step[Unit] =
    ???

  def checkExprStmt(stmt: TExprStmt): Step[Unit] =
    ???

  def checkSelect(select: TSelectExpr): Step[Type] =
    ???

  def checkCond(cond: TCondExpr): Step[Type] =
    ???

  def checkCast(cast: TCastExpr): Step[Type] =
    ???

  def checkObj(obj: TObjExpr): Step[Type] =
    ???

  def checkArr(arr: TArrExpr): Step[Type] =
    ???

  def pure[A](value: A): Step[A] =
    value.pure[Step]
}