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

  import Value._

  implicit def valueEncoder: ValueEncoder[Value] =
    pure(identity)

  implicit def listEncoder[A](implicit enc: ValueEncoder[A]): ValueEncoder[List[A]] =
    pure(list => Arr(list.map(enc.apply)))

  implicit def circeEncoder[A](implicit enc: Encoder[A]): ValueEncoder[A] =
    pure { arg =>
      def toValue(json: Json): Value =
        json.fold(
          jsonNull    = Null,
          jsonBoolean = bool  => if(bool) True else False,
          jsonNumber  = num   => num.toInt.fold[Value](Real(num.toDouble))(Intr),
          jsonString  = str   => Str(str),
          jsonArray   = items => Arr(items.toList.map(toValue)),
          jsonObject  = obj   => Obj(obj.toList.map { case (n, j) => (n, toValue(j)) })
        )

      toValue(arg.asJson)
    }
}
