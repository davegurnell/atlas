package atlas

import cats.{Monoid, Semigroup, Show}
import cats.implicits._

sealed abstract class Type extends Product with Serializable

final case class TypeVar(id: Int) extends Type
final case class TypeRef(name: String) extends Type

final case class FuncType(argTypes: List[Type], returnType: Type) extends Type
// final case class UnionType(types: Set[Type]) extends Type

sealed abstract class ConcreteType extends Type
// final case class ObjType(fieldTypes: List[(String, Type)]) extends Type
final case class ArrType(itemType: Type) extends Type
case object StrType extends ConcreteType
case object IntType extends ConcreteType
case object DblType extends ConcreteType
case object BoolType extends ConcreteType
case object NullType extends ConcreteType

case object AnyType extends ConcreteType // added to cope with empty arrays

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

  implicit val typeShow: Show[Type] =
    Show.show {
      case TypeVar(i)      => show"v$i"
      case TypeRef(n)      => n
      case FuncType(as, r) => show"(${as.map(_.show).mkString(", ")}) -> $r"
      // case UnionType(ts) =>
      // case ObjType()     =>
      case ArrType(t)      => show"$t[]"
      case StrType         => "String"
      case IntType         => "Int"
      case DblType         => "Real"
      case BoolType        => "Bool"
      case NullType        => "Null"
      case AnyType         => "Any"
    }
}
