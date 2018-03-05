package atlas

object syntax {
  implicit class ValueEncoderOps[A](a: A) {
    def toValue(implicit enc: ValueEncoder[A]): Value =
      enc(a)
  }

  implicit class ValueDecoderOps(value: Value) {
    def as[A](implicit dec: ValueDecoder[A]): Either[String, A] =
      dec(value)
  }
}
