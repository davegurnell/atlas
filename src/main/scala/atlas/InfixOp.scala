package atlas

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
