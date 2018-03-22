package atlas

import io.circe.syntax._
import io.circe.{Encoder, Json}

trait TypeEncoder[A] {
  def get: Type
}

object TypeEncoder extends TypeEncoderFunctions
  with TypeEncoderInstances
  with TypeEncoderBoilerplate

trait TypeEncoderFunctions {
  def get[A](implicit enc: TypeEncoder[A]): TypeEncoder[A] =
    enc

  def pure[A](tpe: Type): TypeEncoder[A] =
    new TypeEncoder[A] {
      def get: Type =
        tpe
    }
}

trait TypeEncoderInstances extends TypeEncoderLowPriorityInstances {
  self: TypeEncoderFunctions =>

  import Type._

  implicit def value: TypeEncoder[Value] =
    pure(Type.Top)

  implicit def boolean: TypeEncoder[Boolean] =
    pure(Type.Bool)

  implicit def int: TypeEncoder[Int] =
    pure(Type.Intr)

  implicit def double: TypeEncoder[Double] =
    pure(Type.Real)

  implicit def string: TypeEncoder[String] =
    pure(Type.Str)

  implicit def list[A](implicit enc: TypeEncoder[A]): TypeEncoder[List[A]] =
    pure(Arr(enc.get))
}

trait TypeEncoderLowPriorityInstances {
  self: TypeEncoderFunctions =>

  import Type._
}
