package atlas

sealed abstract class Expr extends Product with Serializable

final case class RefExpr(id: String) extends Expr
final case class LetExpr(varName: String, varType: Option[Type], expr: Expr) extends Expr

final case class AppExpr(func: Expr, args: List[Expr]) extends Expr
final case class InfixExpr(op: InfixOp, arg1: Expr, arg2: Expr) extends Expr
final case class PrefixExpr(op: PrefixOp, arg: Expr) extends Expr

final case class FuncExpr(args: List[FuncArg], resultType: Option[Type], body: Expr) extends Expr
final case class FuncArg(argName: String, argType: Option[Type] = None)

final case class BlockExpr(stmts: List[Expr], expr: Expr) extends Expr
// final case class Select(expr: Expr, field: String) extends Expr
final case class CondExpr(test: Expr, trueArm: Expr, falseArm: Expr) extends Expr
final case class CastExpr(expr: Expr, asType: Type) extends Expr

// final case class Obj(fields: List[(String, Expr)]) extends Expr
// final case class Arr(items: List[Expr]) extends Expr
final case class StrExpr(value: String) extends Expr
final case class IntExpr(value: Int) extends Expr
final case class DblExpr(value: Double) extends Expr
final case class BoolExpr(value: Boolean) extends Expr
final case object NullExpr extends Expr
