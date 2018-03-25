package atlas

import cats.implicits._

trait ValueDecoder[A] {
  def apply(value: Value): Either[RuntimeError, A]
}

object ValueDecoder extends ValueDecoderFunctions
  with ValueDecoderInstances
  with ValueDecoderBoilerplate

trait ValueDecoderFunctions {
  def apply[A](implicit instance: ValueDecoder[A]): ValueDecoder[A] =
    instance

  def pure[A](func: Value => Either[RuntimeError, A]): ValueDecoder[A] =
    new ValueDecoder[A] {
      def apply(arg: Value): Either[RuntimeError, A] =
        func(arg)
    }
}

trait ValueDecoderInstances {
  self: ValueDecoderFunctions =>

  implicit def valueDecoder: ValueDecoder[Value] =
    pure(_.asRight)

  implicit def listDecoder[A](implicit dec: ValueDecoder[A]): ValueDecoder[List[A]] =
    pure {
      case ArrVal(values) => values.traverse(dec.apply)
      case value          => Left(RuntimeError(s"Could not decode list: $value"))
    }

  import io.circe.{Decoder, Json}

  implicit def circeDecoder[A](implicit dec: Decoder[A]): ValueDecoder[A] =
    pure { value =>
      def toJson(value: Value): Either[RuntimeError, Json] =
        value match {
          case NullVal        => Right(Json.Null)
          case BoolVal(value) => Right(if(value) Json.True else Json.False)
          case IntVal(num)    => Right(Json.fromInt(num))
          case DblVal(num)    => Json.fromDouble(num).toRight(RuntimeError(s"Could not decode double: $num"))
          case StrVal(str)    => Right(Json.fromString(str))
          case ArrVal(items)  => items.traverse(toJson).map(Json.fromValues)
          case ObjVal(fields) => fields.traverse { case (n, v) => toJson(v).map(j => (n, j)) }.map(Json.fromFields)
          case value: FuncVal => Left(RuntimeError(s"Cannot decode function: $value"))
        }

      toJson(value).flatMap(_.as[A].leftMap(err => RuntimeError(err.message)))
    }
}
