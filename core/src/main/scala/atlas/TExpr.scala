package atlas

sealed abstract class TExpr extends Product with Serializable { def tpe: TypeVar }

final case class TRefExpr(tpe: TypeVar, id: String) extends TExpr

final case class TAppExpr(tpe: TypeVar, func: TExpr, args: List[TExpr]) extends TExpr
final case class TInfixExpr(tpe: TypeVar, op: InfixOp, arg1: TExpr, arg2: TExpr) extends TExpr
final case class TPrefixExpr(tpe: TypeVar, op: PrefixOp, arg: TExpr) extends TExpr

final case class TFuncExpr(tpe: TypeVar, args: List[TFuncArg], resType: Option[Type], body: TExpr) extends TExpr
final case class TFuncArg(tpe: TypeVar, argName: String, argType: Option[Type])

final case class TBlockExpr(tpe: TypeVar, stmts: List[TStmt], expr: TExpr) extends TExpr
final case class TSelectExpr(tpe: TypeVar, expr: TExpr, field: String) extends TExpr
final case class TCondExpr(tpe: TypeVar, test: TExpr, trueArm: TExpr, falseArm: TExpr) extends TExpr
final case class TCastExpr(tpe: TypeVar, expr: TExpr, asType: Type) extends TExpr
final case class TParenExpr(tpe: TypeVar, expr: TExpr) extends TExpr

final case class TObjExpr(tpe: TypeVar, fields: List[(String, TExpr)]) extends TExpr
final case class TArrExpr(tpe: TypeVar, exprs: List[TExpr]) extends TExpr
final case class TStrExpr(tpe: TypeVar, value: String) extends TExpr
final case class TIntExpr(tpe: TypeVar, value: Int) extends TExpr
final case class TDblExpr(tpe: TypeVar, value: Double) extends TExpr
final case class TBoolExpr(tpe: TypeVar, value: Boolean) extends TExpr
final case class TNullExpr(tpe: TypeVar) extends TExpr

sealed abstract class TStmt extends Product with Serializable { def tpe: TypeVar }
final case class TLetStmt(tpe: TypeVar, varName: String, varType: Option[Type], expr: TExpr) extends TStmt
final case class TExprStmt(tpe: TypeVar, expr: TExpr) extends TStmt
