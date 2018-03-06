package atlas

import cats.implicits._
import io.circe.{Encoder, Json}
import io.circe.syntax._

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
    pure(list => ArrayValue(list.map(enc.apply)))

  implicit def circeEncoder[A](implicit enc: Encoder[A]): ValueEncoder[A] =
    pure { arg =>
      def toValue(json: Json): Value =
        json.fold(
          jsonNull    = NullValue,
          jsonBoolean = bool  => if(bool) TrueValue else FalseValue,
          jsonNumber  = num   => num.toInt.fold[Value](DoubleValue(num.toDouble))(IntValue),
          jsonString  = StringValue,
          jsonArray   = items => ArrayValue(items.toList.map(toValue)),
          jsonObject  = obj   => ObjectValue(obj.toList.map { case (n, j) => (n, toValue(j)) })
        )

      toValue(arg.asJson)
    }
}
