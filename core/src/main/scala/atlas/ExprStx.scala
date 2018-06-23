package atlas

sealed abstract class ExprStx extends Product with Serializable {
  def desugar: Expr =
    Desugarer.desugar(this)
}

final case class RefExprStx(name: String) extends ExprStx

final case class AppExprStx(func: ExprStx, args: List[ExprStx]) extends ExprStx
final case class InfixExprStx(op: InfixOp, arg1: ExprStx, arg2: ExprStx) extends ExprStx
final case class PrefixExprStx(op: PrefixOp, arg: ExprStx) extends ExprStx

final case class FuncExprStx(typeVars: List[String], args: List[FuncArgStx], resType: Option[TypeStx], body: ExprStx) extends ExprStx
final case class FuncArgStx(name: String, tpe: Option[TypeStx])

final case class BlockExprStx(stmts: List[StmtStx], expr: ExprStx) extends ExprStx
final case class SelectExprStx(expr: ExprStx, field: String) extends ExprStx
final case class CondExprStx(test: ExprStx, trueArm: ExprStx, falseArm: ExprStx) extends ExprStx
final case class CastExprStx(expr: ExprStx, asType: TypeStx) extends ExprStx
final case class ParenExprStx(expr: ExprStx) extends ExprStx

final case class ObjExprStx(fields: List[(String, ExprStx)]) extends ExprStx
final case class ArrExprStx(exprs: List[ExprStx]) extends ExprStx
final case class StrExprStx(value: String) extends ExprStx
final case class IntExprStx(value: Int) extends ExprStx
final case class DblExprStx(value: Double) extends ExprStx
final case class BoolExprStx(value: Boolean) extends ExprStx
case object NullExprStx extends ExprStx

sealed abstract class StmtStx extends Product with Serializable
final case class LetStmtStx(name: String, tpe: Option[TypeStx], expr: ExprStx) extends StmtStx
final case class TypeStmtStx(name: String, tpe: TypeStx) extends StmtStx
final case class ExprStmtStx(expr: ExprStx) extends StmtStx

sealed abstract class TypeStx extends Product with Serializable
final case class RefTypeStx(name: String) extends TypeStx
final case class FuncTypeStx(typeVars: List[String], argTypes: List[TypeStx], resType: TypeStx) extends TypeStx
final case class UnionTypeStx(a: TypeStx, b: TypeStx) extends TypeStx
final case class NullableTypeStx(arg: TypeStx) extends TypeStx
final case class ObjTypeStx(fields: List[(String, TypeStx)]) extends TypeStx
final case class ArrTypeStx(arg: TypeStx) extends TypeStx
final case class ParenTypeStx(tpe: TypeStx) extends TypeStx
case object StrTypeStx extends TypeStx
case object IntTypeStx extends TypeStx
case object DblTypeStx extends TypeStx
case object BoolTypeStx extends TypeStx
case object NullTypeStx extends TypeStx
