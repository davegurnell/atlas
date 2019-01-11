package atlas

trait PrefixImpl {
  private val pos: Native =
    Native((a: Int) => a) orElse
    Native((a: Double) => a)

  private val neg: Native =
    Native((a: Int) => -a) orElse
    Native((a: Double) => -a)

  private val not: Native =
    Native((a: Boolean) => !a)

  def prefixImpl(op: PrefixOp): Native =
    op match {
      case PrefixOp.Pos => pos
      case PrefixOp.Neg => neg
      case PrefixOp.Not => not
    }
}
