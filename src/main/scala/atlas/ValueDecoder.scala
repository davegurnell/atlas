package atlas

import cats.implicits._

trait ValueDecoder[A] {
  def apply(value: Value): Either[String, A]
}

object ValueDecoder extends ValueDecoderFunctions
  with ValueDecoderInstances
  with ValueDecoderBoilerplate

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

  import Value._

  implicit def valueDecoder: ValueDecoder[Value] =
    pure(_.asRight)

  implicit def listDecoder[A](implicit dec: ValueDecoder[A]): ValueDecoder[List[A]] =
    pure {
      case Arr(values) => values.traverse(dec.apply)
      case value       => Left(s"Could not decode list: $value")
    }

  import io.circe.{Decoder, Json}

  implicit def circeDecoder[A](implicit dec: Decoder[A]): ValueDecoder[A] =
    pure { value =>
      def toJson(value: Value): Either[String, Json] =
        value match {
          case Null        => Right(Json.Null)
          case True        => Right(Json.True)
          case False       => Right(Json.False)
          case Intr(num)   => Right(Json.fromInt(num))
          case Real(num)   => Json.fromDouble(num).toRight(s"Could not decode double: $num")
          case Str(str)    => Right(Json.fromString(str))
          case Arr(items)  => items.traverse(toJson).map(Json.fromValues)
          case Obj(fields) => fields.traverse { case (n, v) => toJson(v).map(j => (n, j)) }.map(Json.fromFields)
          case value: Func => Left(s"Cannot decode function: $value")
        }

      toJson(value).flatMap(_.as[A].leftMap(_.message))
    }
}
