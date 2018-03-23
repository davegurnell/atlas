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

object TypeVar {
  implicit val ordering: Ordering[TypeVar] =
    Ordering.by[TypeVar, Int](_.id)
}

object Type {
  implicit val ordering: Ordering[Type] =
    Ordering.fromLessThan[Type] {
      case (a: TypeVar, b: TypeVar) => a.id < b.id
      case (a, b: TypeVar)          => true
      case (a: TypeVar, b)          => false
      case (a, b)                   => a.toString < b.toString
    }
}
