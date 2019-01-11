package atlas

trait ValueEncoder[A] {
  def apply(value: A): Value
}

object ValueEncoder extends ValueEncoderFunctions
  with ValueEncoderInstances

trait ValueEncoderFunctions {
  def apply[A](implicit enc: ValueEncoder[A]): ValueEncoder[A] =
    enc

  def pure[A](func: A => Value): ValueEncoder[A] =
    new ValueEncoder[A] {
      def apply(arg: A): Value =
        func(arg)
    }
}

trait ValueEncoderInstances {
  self: ValueEncoderFunctions =>

  implicit def value[A <: Value]: ValueEncoder[A] =
    pure(value => value)

  implicit val unit: ValueEncoder[Unit] =
    pure(value => NullVal)

  implicit val boolean: ValueEncoder[Boolean] =
    pure(value => BoolVal(value))

  implicit val int: ValueEncoder[Int] =
    pure(value => IntVal(value))

  implicit val double: ValueEncoder[Double] =
    pure(value => DblVal(value))

  implicit val string: ValueEncoder[String] =
    pure(value => StrVal(value))

  implicit def list[A](implicit enc: ValueEncoder[A]): ValueEncoder[List[A]] =
    pure(list => ArrVal(list.map(enc.apply)))
}
