package atlas

object Ast {
  sealed abstract class Stmt extends Product with Serializable
  final case class Defn(ref: Ref, expr: Expr) extends Stmt
  sealed abstract class Expr extends Stmt

  final case class Ref(id: String) extends Expr
  final case class Block(stmts: List[Stmt], expr: Expr) extends Expr
  final case class Select(expr: Expr, ref: Ref) extends Expr
  final case class Cond(test: Expr, trueArm: Expr, falseArm: Expr) extends Expr
  final case class Infix(op: InfixOp, arg1: Expr, arg2: Expr) extends Expr
  final case class Prefix(op: PrefixOp, arg: Expr) extends Expr
  final case class Apply(func: Expr, args: List[Expr]) extends Expr

  sealed abstract class Literal extends Expr

  final case class FuncLiteral(args: List[Ref], body: Expr) extends Literal {
    def arity: Int = args.length
  }

  sealed abstract class DataLiteral extends Literal
  final case class ObjectLiteral(fields: List[(String, Expr)]) extends DataLiteral
  final case class ArrayLiteral(items: List[Expr]) extends DataLiteral
  final case class StringLiteral(value: String) extends DataLiteral
  final case class IntLiteral(value: Int) extends DataLiteral
  final case class DoubleLiteral(value: Double) extends DataLiteral
  final case object NullLiteral extends DataLiteral
  sealed abstract class BooleanLiteral extends DataLiteral
  final case object TrueLiteral extends BooleanLiteral
  final case object FalseLiteral extends BooleanLiteral
}

sealed abstract class InfixOp(val id: String) extends Product with Serializable

object InfixOp {
  case object Add extends InfixOp("+")
  case object Sub extends InfixOp("-")
  case object Mul extends InfixOp("*")
  case object Div extends InfixOp("/")
  case object And extends InfixOp("&&")
  case object Or extends InfixOp("||")
  case object Eq extends InfixOp("==")
  case object Ne extends InfixOp("!=")
  case object Gt extends InfixOp(">")
  case object Lt extends InfixOp("<")
  case object Gte extends InfixOp(">=")
  case object Lte extends InfixOp("<=")
}

sealed abstract class PrefixOp(val id: String) extends Product with Serializable

object PrefixOp {
  case object Not extends PrefixOp("!")
  case object Pos extends PrefixOp("+")
  case object Neg extends PrefixOp("-")
}
