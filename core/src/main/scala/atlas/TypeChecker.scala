package atlas

import cats.MonadError
import cats.data.StateT
import cats.instances.either._
import cats.instances.list._
import cats.syntax.all._

object TypeChecker {
  type Out[A] = Either[TypeError, A]

  type Step[A] = TypeStep[Out, A]

  def check(expr: TExpr, env: TypeEnv = TypeEnv.create): Out[Type] =
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
    getVariable(ref.id)

  def checkApp(apply: TAppExpr): Step[Type] =
    ???

  def checkInfix(infix: TInfixExpr): Step[Type] =
    ???

  def checkPrefix(prefix: TPrefixExpr): Step[Type] =
    ???

  def checkFunc(func: TFuncExpr): Step[Type] =
    ???

  def checkBlock(block: TBlockExpr): Step[Type] =
    for {
      _   <- checkStmts(block.stmts)
      ans <- checkExpr(block.expr)
    } yield ans

  def checkStmts(stmts: List[TStmt]): Step[Unit] =
    stmts.traverse(checkStmt).map(_ => ())

  def checkStmt(stmt: TStmt): Step[Unit] =
    stmt match {
      case stmt: TLetStmt  => checkLetStmt(stmt)
      case stmt: TExprStmt => checkExpr(stmt.expr).map(_ => ())
    }

  def checkLetStmt(stmt: TLetStmt): Step[Unit] =
    checkExpr(stmt.expr).flatMap(setVariable(stmt.varName, _))

  def checkExprStmt(stmt: TExprStmt): Step[Unit] =
    ???

  def checkSelect(select: TSelectExpr): Step[Type] =
    ???

  def checkCond(cond: TCondExpr): Step[Type] =
    for {
      test     <- checkExpr(cond.test)
      _        <- assertAssignable(BoolType, test)
      trueArm  <- checkExpr(cond.trueArm)
      falseArm <- checkExpr(cond.falseArm)
    } yield Type.union(trueArm, falseArm)

  def checkCast(cast: TCastExpr): Step[Type] =
    for {
      expr <- checkExpr(cast.expr)
      _    <- assertAssignable(cast.asType, expr)
    } yield cast.asType

  def checkObj(obj: TObjExpr): Step[Type] =
    ???

  def checkArr(arr: TArrExpr): Step[Type] =
    arr.exprs.traverse(checkExpr).map(types => ArrType(Type.unionAll(types)))

  def pure[A](value: A): Step[A] =
    value.pure[Step]

  def fail[A](error: TypeError): Step[A] =
    error.raiseError[Step, A]

  def assertAssignable(to: Type, from: Type): Step[Unit] =
    if(Type.isAssignable(to, from)) {
      pure(())
    } else {
      fail(TypeError.typeMismatch(to, from))
    }

  def getVariable(name: String): Step[Type] =
    inspectEnv(env => env.get(name) match {
      case Some(tpe) => tpe.pure[Out]
      case None      => TypeError.variableNotFound(name).raiseError[Out, Type]
    })

  def setVariable(name: String, value: Type): Step[Unit] =
    inspectEnv(env => env.destructiveSet(name, value).pure[Out])

  def inspectEnv[A](func: TypeEnv => Out[A]): Step[A] =
    StateT.inspectF(func)

  def updateEnv(func: TypeEnv => Out[TypeEnv]): Step[Unit] =
    StateT.modifyF(func)
}