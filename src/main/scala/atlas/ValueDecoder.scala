package atlas

import cats.implicits._
import io.circe.{Decoder, Json}
import io.circe.syntax._

trait ValueDecoder[A] {
  def apply(value: Value): Either[String, A]
}

object ValueDecoder extends ValueDecoderFunctions
  with ValueDecoderInstances

trait ValueDecoderFunctions {
  def apply[A](implicit instance: ValueDecoder[A]): ValueDecoder[A] =
    instance

  def pure[A](func: Value => Either[String, A]): ValueDecoder[A] =
    new ValueDecoder[A] {
      def apply(arg: Value): Either[String, A] =
        func(arg)
    }
}

trait ValueDecoderInstances {
  self: ValueDecoderFunctions =>

  implicit def valueDecoder: ValueDecoder[Value] =
    pure(_.asRight)

  implicit def circeDecoder[A](implicit dec: Decoder[A]): ValueDecoder[A] =
    pure { value =>
      def toJson(value: Value): Either[String, Json] =
        value match {
          case NullValue           => Right(Json.Null)
          case TrueValue           => Right(Json.True)
          case FalseValue          => Right(Json.False)
          case IntValue(num)       => Right(Json.fromInt(num))
          case DoubleValue(num)    => Json.fromDouble(num).toRight(s"Could not decode double: $num")
          case StringValue(str)    => Right(Json.fromString(str))
          case ArrayValue(items)   => items.traverse(toJson).map(Json.fromValues)
          case ObjectValue(fields) => fields.traverse { case (n, v) => toJson(v).map(j => (n, j)) }.map(Json.fromFields)
          case value: FuncValue    => Left(s"Cannot decode function: $value")
        }

      toJson(value).flatMap(_.as[A].leftMap(_.message))
    }
}
