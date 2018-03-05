package atlas

object PrefixFunc {
  val pos: NativeFunc =
    NativeFunc((a: Int) => a) orElse
    NativeFunc((a: Double) => a)

  val neg: NativeFunc =
    NativeFunc((a: Int) => -a) orElse
    NativeFunc((a: Double) => -a)

  val not: NativeFunc =
    NativeFunc((a: Boolean) => !a)

  def apply(op: PrefixOp): NativeFunc =
    op match {
      case PrefixOp.Pos => pos
      case PrefixOp.Neg => neg
      case PrefixOp.Not => not
    }
}
