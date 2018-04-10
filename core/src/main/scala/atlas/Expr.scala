package atlas

sealed abstract class Expr extends Product with Serializable

final case class RefExpr(id: String) extends Expr

final case class AppExpr(func: Expr, args: List[Expr]) extends Expr
final case class InfixExpr(op: InfixOp, arg1: Expr, arg2: Expr) extends Expr
final case class PrefixExpr(op: PrefixOp, arg: Expr) extends Expr

final case class FuncExpr(args: List[FuncArg], retType: Option[Type], body: Expr) extends Expr
final case class FuncArg(argName: String, argType: Option[Type])

final case class BlockExpr(stmts: List[Stmt], expr: Expr) extends Expr
final case class SelectExpr(expr: Expr, field: String) extends Expr
final case class CondExpr(test: Expr, trueArm: Expr, falseArm: Expr) extends Expr
final case class CastExpr(expr: Expr, asType: Type) extends Expr
final case class ParenExpr(expr: Expr) extends Expr

final case class ObjExpr(fields: List[(String, Expr)]) extends Expr
final case class ArrExpr(exprs: List[Expr]) extends Expr
final case class StrExpr(value: String) extends Expr
final case class IntExpr(value: Int) extends Expr
final case class DblExpr(value: Double) extends Expr
final case class BoolExpr(value: Boolean) extends Expr
case object NullExpr extends Expr

sealed abstract class Stmt extends Product with Serializable
final case class LetStmt(varName: String, varType: Option[Type], expr: Expr) extends Stmt
final case class LetTypeStmt(typeName: String, asType: Type) extends Stmt
final case class ExprStmt(expr: Expr) extends Stmt

sealed abstract class TypeExpr extends Product with Serializable
final case class RefTypeExpr(name: String) extends TypeExpr
final case class FuncTypeExpr(argTypes: List[TypeExpr], resType: Type) extends TypeExpr
final case class UnionTypeExpr(types: List[TypeExpr]) extends TypeExpr
final case class ObjTypeExpr(fields: List[(String, TypeExpr)]) extends TypeExpr
final case class ArrTypeExpr(items: List[TypeExpr]) extends TypeExpr
case object StrTypeExpr extends TypeExpr
case object IntTypeExpr extends TypeExpr
case object DblTypeExpr extends TypeExpr
case object BoolTypeExpr extends TypeExpr
case object NullTypeExpr extends TypeExpr
