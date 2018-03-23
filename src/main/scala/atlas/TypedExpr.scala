package atlas

sealed abstract class TExpr extends Product with Serializable {
  def tpe: TypeVar
}

final case class TRefExpr(tpe: TypeVar, id: String) extends TExpr
final case class TLetExpr(tpe: TypeVar, varName: String, varType: Option[Type], expr: TExpr) extends TExpr

final case class TAppExpr(tpe: TypeVar, func: TExpr, args: List[TExpr]) extends TExpr
final case class TInfixExpr(tpe: TypeVar, op: InfixOp, arg1: TExpr, arg2: TExpr) extends TExpr
final case class TPrefixExpr(tpe: TypeVar, op: PrefixOp, arg: TExpr) extends TExpr

final case class TFuncExpr(tpe: TypeVar, args: List[TFuncArg], resultType: Option[Type], body: TExpr) extends TExpr
final case class TFuncArg(tpe: TypeVar, argName: String, argType: Option[Type] = None)

final case class TBlockExpr(tpe: TypeVar, stmts: List[TExpr], expr: TExpr) extends TExpr
// final case class Select(tpe: TypeVar, expr: TExpr, field: String) extends TExpr
final case class TCondExpr(tpe: TypeVar, test: TExpr, trueArm: TExpr, falseArm: TExpr) extends TExpr
final case class TCastExpr(tpe: TypeVar, expr: TExpr, asType: Type) extends TExpr

// final case class TObj(tpe: TypeVar, fields: List[(String, Expr)]) extends TExpr
// final case class TArr(tpe: TypeVar, items: List[Expr]) extends TExpr
final case class TStrExpr(tpe: TypeVar, value: String) extends TExpr
final case class TIntExpr(tpe: TypeVar, value: Int) extends TExpr
final case class TDblExpr(tpe: TypeVar, value: Double) extends TExpr
final case class TBoolExpr(tpe: TypeVar, value: Boolean) extends TExpr
final case class TNullExpr(tpe: TypeVar) extends TExpr
