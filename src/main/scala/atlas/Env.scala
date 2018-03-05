package atlas

final case class Env(bindings: List[(String, Value)]) extends Product with Serializable {
  def get(id: String): Option[Value] =
    bindings.collectFirst { case (`id`, v) => v }

  def set[A](id: String, value: A)(implicit enc: ValueEncoder[A]): Env =
    Env((id, enc(value)) :: bindings)

  def setAll(bindings: Seq[(String, Value)]): Env =
    bindings.foldLeft(this) { (env, binding) =>
      val (id, value) = binding
      env.set(id, value)
    }
}

object Env {
  val empty: Env = Env(Nil)
}
