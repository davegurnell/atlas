package atlas

sealed abstract class Stmt extends Product with Serializable
final case class LetStmt(varName: String, expr: Expr) extends Stmt
final case class ExprStmt(expr: Expr) extends Stmt

sealed abstract class Expr extends Product with Serializable

final case class RefExpr(id: String) extends Expr

final case class AppExpr(func: Expr, args: List[Expr]) extends Expr
final case class InfixExpr(op: InfixOp, arg1: Expr, arg2: Expr) extends Expr
final case class PrefixExpr(op: PrefixOp, arg: Expr) extends Expr

final case class FuncExpr(args: List[FuncArg], body: Expr) extends Expr
final case class FuncArg(argName: String)

final case class BlockExpr(stmts: List[Stmt], expr: Expr) extends Expr
final case class SelectExpr(expr: Expr, field: String) extends Expr
final case class CondExpr(test: Expr, trueArm: Expr, falseArm: Expr) extends Expr
final case class ParenExpr(expr: Expr) extends Expr

final case class ObjExpr(fields: List[(String, Expr)]) extends Expr
final case class ArrExpr(exprs: List[Expr]) extends Expr
final case class StrExpr(value: String) extends Expr
final case class IntExpr(value: Int) extends Expr
final case class DblExpr(value: Double) extends Expr
final case class BoolExpr(value: Boolean) extends Expr
final case object NullExpr extends Expr
