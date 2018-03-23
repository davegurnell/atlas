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

trait TypeEncoderInstances {
  self: TypeEncoderFunctions =>

  implicit def value: TypeEncoder[Value] =
    // pure(TopType)
    ???

  implicit def boolean: TypeEncoder[Boolean] =
    pure(BoolType)

  implicit def int: TypeEncoder[Int] =
    pure(IntType)

  implicit def double: TypeEncoder[Double] =
    pure(DblType)

  implicit def string: TypeEncoder[String] =
    pure(StrType)

  // implicit def list[A](implicit enc: TypeEncoder[A]): TypeEncoder[List[A]] =
  //   pure(ArrType(enc.get))
}
