package atlas

import cats.{Monoid, Semigroup}

sealed abstract class Type extends Product with Serializable

object Type {
  final case class Var(id: Int) extends Type
  final case class Ref(name: String) extends Type
  final case class Func(args: List[Type], result: Type) extends Type
  final case class Union(types: Set[Type]) extends Type

  sealed abstract class Data extends Type
  final case class Obj(fieldTypes: List[(String, Type)]) extends Data
  final case class Arr(itemType: Type) extends Data
  case object Str extends Data
  case object Intr extends Data
  case object Real extends Data
  case object Null extends Data
  case object Bool extends Data

  case object Bottom extends Type
  case object Top extends Type

  val bottom: Type = Bottom
  val top: Type = Top

  def union(a: Type, b: Type): Type =
    (a, b) match {
      case (a, Top)             => Top
      case (Top, b)             => Top
      case (a, Bottom)          => a
      case (Bottom, b)          => b
      case (a, b) if a == b     => a
      case (Union(a), Union(b)) => Union(a ++ b)
      case (Union(a), b)        => Union(a + b)
      case (a, Union(b))        => Union(b + a)
      case (a, b)               => Union(Set(a, b))
    }

  implicit val monoid: Monoid[Type] =
    new Monoid[Type] {
      override def empty: Type =
        Bottom

      override def combine(x: Type, y: Type): Type =
        union(x, y)
    }
}
