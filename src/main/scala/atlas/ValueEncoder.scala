package atlas

import io.circe.syntax._
import io.circe.{Encoder, Json}

trait ValueEncoder[A] {
  def apply(value: A): Value
}

object ValueEncoder extends ValueEncoderFunctions
  with ValueEncoderInstances
  with ValueEncoderBoilerplate

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

  implicit def valueEncoder: ValueEncoder[Value] =
    pure(identity)

  implicit def listEncoder[A](implicit enc: ValueEncoder[A]): ValueEncoder[List[A]] =
    pure(list => ArrVal(list.map(enc.apply)))

  implicit def circeEncoder[A](implicit enc: Encoder[A]): ValueEncoder[A] =
    pure { arg =>
      def toAtlas(json: Json): Value =
        json.fold(
          jsonNull    = NullVal,
          jsonBoolean = bool  => BoolVal(bool),
          jsonNumber  = num   => num.toInt.fold[Value](DblVal(num.toDouble))(IntVal),
          jsonString  = str   => StrVal(str),
          jsonArray   = items => ArrVal(items.toList.map(toAtlas)),
          jsonObject  = obj   => ObjVal(obj.toList.map { case (n, j) => (n, toAtlas(j)) })
        )

      toAtlas(arg.asJson)
    }
}
