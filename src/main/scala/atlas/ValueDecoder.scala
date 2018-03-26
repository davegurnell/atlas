package atlas

import cats.{Applicative, ApplicativeError, MonadError, Traverse}
import cats.implicits._

trait ValueDecoder[F[_], A] {
  def apply(value: Value[F]): F[A]
}

object ValueDecoder extends ValueDecoderFunctions
  with ValueDecoderInstances
  with ValueDecoderBoilerplate

trait ValueDecoderFunctions {
  def apply[F[_], A](implicit instance: ValueDecoder[F, A]): ValueDecoder[F, A] =
    instance

  def pure[F[_], A](func: Value[F] => F[A]): ValueDecoder[F, A] =
    new ValueDecoder[F, A] {
      def apply(arg: Value[F]): F[A] =
        func(arg)
    }
}

trait ValueDecoderInstances {
  self: ValueDecoderFunctions =>

  implicit def value[F[_]](implicit app: Applicative[F]): ValueDecoder[F, Value[F]] =
    pure(_.pure[F])

  implicit def boolean[F[_]](implicit app: ApplicativeError[F, RuntimeError]): ValueDecoder[F, Boolean] =
    pure {
      case BoolVal(value) => value.pure[F]
      case value          => RuntimeError(s"Could not decode boolean: $value").raiseError[F, Boolean]
    }

  implicit def int[F[_]](implicit app: ApplicativeError[F, RuntimeError]): ValueDecoder[F, Int] =
    pure {
      case IntVal(value) => value.pure[F]
      case value         => RuntimeError(s"Could not decode int: $value").raiseError[F, Int]
    }

  implicit def double[F[_]](implicit app: ApplicativeError[F, RuntimeError]): ValueDecoder[F, Double] =
    pure {
      case DblVal(value) => value.pure[F]
      case value         => RuntimeError(s"Could not decode double: $value").raiseError[F, Double]
    }

  implicit def string[F[_]](implicit app: ApplicativeError[F, RuntimeError]): ValueDecoder[F, String] =
    pure {
      case StrVal(value) => value.pure[F]
      case value         => RuntimeError(s"Could not decode string: $value").raiseError[F, String]
    }

  implicit def list[F[_], A](implicit app: ApplicativeError[F, RuntimeError], dec: ValueDecoder[F, A]): ValueDecoder[F, List[A]] =
    pure {
      case ArrVal(values) => values.traverse(dec.apply)
      case value          => RuntimeError(s"Could not decode list: $value").raiseError[F, List[A]]
    }

  import io.circe.{Decoder, Json}

  implicit def circe[F[_], A](implicit monad: MonadError[F, RuntimeError]): ValueDecoder[F, Json] =
    pure { value =>
      def toJson(value: Value[F]): F[Json] =
        value match {
          case NullVal()         => Json.Null.pure[F]
          case BoolVal(value)    => (if(value) Json.True else Json.False).pure[F]
          case IntVal(num)       => Json.fromInt(num).pure[F]
          case DblVal(num)       => Json.fromDouble(num).fold(RuntimeError(s"Could not decode double: $num").raiseError[F, Json])(_.pure[F])
          case StrVal(str)       => Json.fromString(str).pure[F]
          case ArrVal(items)     => items.traverse(toJson).map(Json.fromValues)
          case ObjVal(fields)    => fields.traverse { case (n, v) => toJson(v).map(j => (n, j)) }.map(Json.fromFields)
          case value: FuncVal[F] => RuntimeError(s"Cannot decode function as JSON: $value").raiseError[F, Json]
        }

      toJson(value)
    }
}
