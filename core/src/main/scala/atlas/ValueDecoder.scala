package atlas

import cats.implicits._

trait ValueDecoder[A] {
  def apply(value: Value): Either[RuntimeError, A]
}

object ValueDecoder extends ValueDecoderFunctions
  with ValueDecoderInstances

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

  implicit val value: ValueDecoder[Value] =
    pure(Right(_))

  implicit val unit: ValueDecoder[Unit] =
    pure {
      case NullVal => Right(())
      case value   => Left(RuntimeError(s"Could not decode unit: $value"))
    }

  implicit val boolean: ValueDecoder[Boolean] =
    pure {
      case BoolVal(value) => Right(value)
      case value          => Left(RuntimeError(s"Could not decode boolean: $value"))
    }

  implicit val int: ValueDecoder[Int] =
    pure {
      case IntVal(value) => Right(value)
      case value         => Left(RuntimeError(s"Could not decode int: $value"))
    }

  implicit val double: ValueDecoder[Double] =
    pure {
      case IntVal(value) => Right(value * 1.0)
      case DblVal(value) => Right(value)
      case value         => Left(RuntimeError(s"Could not decode double: $value"))
    }

  implicit val string: ValueDecoder[String] =
    pure {
      case StrVal(value) => Right(value)
      case value         => Left(RuntimeError(s"Could not decode string: $value"))
    }

  implicit def list[A](implicit dec: ValueDecoder[A]): ValueDecoder[List[A]] =
    pure {
      case ArrVal(values) => values.traverse(dec.apply)
      case value          => Left(RuntimeError(s"Could not decode list: $value"))
    }
}
