package atlas

trait PrefixImpl[F[_]] {
  self: Interpreter[F] =>

  private val pos: Native[F] =
    native((a: Int) => a) orElse
    native((a: Double) => a)

  private val neg: Native[F] =
    native((a: Int) => -a) orElse
    native((a: Double) => -a)

  private val not: Native[F] =
    native((a: Boolean) => !a)

  def prefixImpl(op: PrefixOp): Native[F] =
    op match {
      case PrefixOp.Pos => pos
      case PrefixOp.Neg => neg
      case PrefixOp.Not => not
    }
}
