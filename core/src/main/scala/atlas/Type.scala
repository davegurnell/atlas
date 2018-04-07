package atlas

import cats.{Monoid, Semigroup, Show}
import cats.implicits._

sealed abstract class Type extends Product with Serializable

final case class TypeVar(id: Int) extends Type
final case class TypeRef(name: String) extends Type

final case class FuncType(argTypes: List[Type], returnType: Type) extends Type
final case class UnionType(types: Set[Type]) extends Type

final case class ObjType(fieldTypes: List[(String, Type)]) extends Type
final case class ArrType(itemType: Type) extends Type

sealed abstract class AtomicType extends Type
case object StrType extends AtomicType
case object IntType extends AtomicType
case object DblType extends AtomicType
case object BoolType extends AtomicType
case object NullType extends AtomicType

object Type {
  implicit val typeVarOrdering: Ordering[TypeVar] =
    Ordering.by[TypeVar, Int](_.id)

  implicit val typeOrdering: Ordering[Type] =
    Ordering.fromLessThan[Type] {
      case (a: TypeVar, b: TypeVar) => a.id < b.id
      case (a, b: TypeVar)          => true
      case (a: TypeVar, b)          => false
      case (a, b)                   => a.toString < b.toString
    }
}
