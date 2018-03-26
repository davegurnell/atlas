package atlas

sealed abstract class PrefixOp(val id: String) extends Product with Serializable

object PrefixOp {
  case object Not extends PrefixOp("!")
  case object Pos extends PrefixOp("+")
  case object Neg extends PrefixOp("-")
}
