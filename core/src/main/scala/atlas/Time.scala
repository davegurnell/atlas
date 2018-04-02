package atlas

object Time {
  def apply[A](name: String)(expr: => A): A = {
    val t0 = System.currentTimeMillis
    try {
      expr
    } finally {
      val t1 = System.currentTimeMillis
      // println(s"$name: ${t1-t0}ms")
    }
  }

}