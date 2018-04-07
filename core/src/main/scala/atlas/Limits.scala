package atlas

case class Limits(
  startTime: Long,
  pc: Int = 0,
  pcLimit: Option[Int] = None,
  runtimeLimit: Option[Long] = None
)

object Limits {
  def create: Limits =
    Limits(System.currentTimeMillis)
}
