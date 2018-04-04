package atlas

trait ValueEncoder[F[_], A] {
  def apply(value: A): Value[F]
}

object ValueEncoder extends ValueEncoderFunctions
  with ValueEncoderInstances

trait ValueEncoderFunctions {
  def apply[F[_], A](implicit enc: ValueEncoder[F, A]): ValueEncoder[F, A] =
    enc

  def pure[F[_], A](func: A => Value[F]): ValueEncoder[F, A] =
    new ValueEncoder[F, A] {
      def apply(arg: A): Value[F] =
        func(arg)
    }
}

trait ValueEncoderInstances {
  self: ValueEncoderFunctions =>

  implicit def value[F[_], A <: Value[F]]: ValueEncoder[F, A] =
    pure(value => value)

  implicit def boolean[F[_]]: ValueEncoder[F, Boolean] =
    pure(value => BoolVal(value))

  implicit def int[F[_]]: ValueEncoder[F, Int] =
    pure(value => IntVal(value))

  implicit def double[F[_]]: ValueEncoder[F, Double] =
    pure(value => DblVal(value))

  implicit def string[F[_]]: ValueEncoder[F, String] =
    pure(value => StrVal(value))

  implicit def list[F[_], A](implicit enc: ValueEncoder[F, A]): ValueEncoder[F, List[A]] =
    pure(list => ArrVal(list.map(enc.apply)))

  // import io.circe.{Encoder, Json}
  // import io.circe.syntax._

  // implicit def circe[F[_]]: ValueEncoder[F, Json] =
  //   pure { arg =>
  //     def toAtlas(json: Json): Value[F] =
  //       json.fold(
  //         jsonNull    = NullVal(),
  //         jsonBoolean = bool  => BoolVal(bool),
  //         jsonNumber  = num   => num.toInt.fold[Value[F]](DblVal(num.toDouble))(IntVal[F]),
  //         jsonString  = str   => StrVal(str),
  //         jsonArray   = items => ArrVal(items.toList.map(toAtlas)),
  //         jsonObject  = obj   => ObjVal(obj.toList.map { case (n, j) => (n, toAtlas(j)) })
  //       )

  //     toAtlas(arg.asJson)
  //   }
}
