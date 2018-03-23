package atlas

import cats.{Monoid, Semigroup}

sealed abstract class Type extends Product with Serializable

final case class TypeVar(id: Int) extends Type
final case class TypeRef(name: String) extends Type

final case class FuncType(argTypes: List[Type], returnType: Type) extends Type
// final case class UnionType(types: Set[Type]) extends Type

sealed abstract class ConcreteType extends Type
// final case class ObjType(fieldTypes: List[(String, Type)]) extends Type
// final case class ArrType(itemType: Type) extends Type
case object StrType extends ConcreteType
case object IntType extends ConcreteType
case object DblType extends ConcreteType
case object NullType extends ConcreteType
case object BoolType extends ConcreteType

// object Type {
//   def union(a: Type, b: Type): Type =
//     (a, b) match {
//       case (a, Top)             => Top
//       case (Top, b)             => Top
//       case (a, Bottom)          => a
//       case (Bottom, b)          => b
//       case (a, b) if a == b     => a
//       case (Union(a), Union(b)) => Union(a ++ b)
//       case (Union(a), b)        => Union(a + b)
//       case (a, Union(b))        => Union(b + a)
//       case (a, b)               => Union(Set(a, b))
//     }

//   implicit val monoid: Monoid[Type] =
//     new Monoid[Type] {
//       override def empty: Type =
//         Bottom

//       override def combine(x: Type, y: Type): Type =
//         union(x, y)
//     }
// }
