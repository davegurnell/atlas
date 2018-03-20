package atlas

object PrefixFunc {
  import Value._

  val pos: Native =
    native((a: Int) => a) orElse
    native((a: Double) => a)

  val neg: Native =
    native((a: Int) => -a) orElse
    native((a: Double) => -a)

  val not: Native =
    native((a: Boolean) => !a)

  def apply(op: PrefixOp): Native =
    op match {
      case PrefixOp.Pos => pos
      case PrefixOp.Neg => neg
      case PrefixOp.Not => not
    }
}
