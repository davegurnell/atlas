package atlas

sealed abstract class Expr extends Product with Serializable

final case class RefExpr(name: String) extends Expr

final case class AppExpr(func: Expr, args: List[Expr]) extends Expr
final case class FuncExpr(typeArgs: List[Type], args: List[FuncArg], resType: Type, body: Expr) extends Expr
final case class FuncArg(name: String, tpe: Type)

final case class BlockExpr(stmts: List[Stmt], expr: Expr) extends Expr
final case class SelectExpr(expr: Expr, field: String) extends Expr
final case class CondExpr(test: Expr, trueArm: Expr, falseArm: Expr) extends Expr
final case class CastExpr(expr: Expr, tpe: Type) extends Expr

final case class ObjExpr(fields: List[(String, Expr)]) extends Expr
final case class ArrExpr(exprs: List[Expr]) extends Expr
final case class StrExpr(value: String) extends Expr
final case class IntExpr(value: Int) extends Expr
final case class DblExpr(value: Double) extends Expr
final case class BoolExpr(value: Boolean) extends Expr
case object NullExpr extends Expr

sealed abstract class Stmt extends Product with Serializable
final case class LetStmt(name: String, tpe: Type, expr: Expr) extends Stmt
final case class TypeStmt(name: String, tpe: Type) extends Stmt
final case class ExprStmt(expr: Expr) extends Stmt
