package atlas

final case class Limits(
  start      : Long,
  maxRuntime : Option[Long] = None,
  maxMemory  : Option[Int]  = None
)

object Limits {
  def create: Limits =
    Limits(System.currentTimeMillis)
}
