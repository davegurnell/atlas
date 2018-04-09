package atlas

import cats.{Monoid, Semigroup, Show}
import cats.implicits._

sealed abstract class Type extends Product with Serializable {
  def |(that: Type): Type =
    Type.union(this, that)

  def ? : Type =
    Type.union(this, NullType)
}

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

object Type extends TypeFunctions with TypeInstances

trait TypeFunctions {
  val emptyUnion: Type =
    UnionType(Set())

  def union(a: Type, b: Type): Type =
    (a, b) match {
      case ( a                , b                ) if a == b => a
      case ( a                , Type.emptyUnion  )           => a
      case ( Type.emptyUnion  , b                )           => b
      case ( UnionType(as)    , UnionType(bs)    )           => UnionType(as ++ bs)
      case ( UnionType(as)    , b                )           => UnionType(as + b)
      case ( a                , UnionType(bs)    )           => UnionType(bs + a)
      case ( a                , b                )           => UnionType(Set(a, b))
    }

  def unionAll(types: Seq[Type]): Type =
    types.foldLeft(UnionType(Set()) : Type)(Type.union)

  def isAssignable(to: Type, from: Type): Boolean =
    (to, from) match {
      case ( a , b             ) if a == b => true
      case ( a , UnionType(bs) )           => bs.forall(isAssignable(a, _))
      case ( UnionType(as) , b )           => as.exists(isAssignable(_, b))
      case ( a , b             )           => false
    }
}

trait TypeInstances {
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