package atlas

import atlas.syntax._

case class Substitution(lhs: TypeVar, rhs: Type)

object Substitution {
  implicit val ordering: Ordering[Substitution] =
    Ordering.by[Substitution, (TypeVar, Type)](c => (c.lhs, c.rhs))
}

object TypeUnifier {
  type Step[A] = Either[TypeError, A]

  def apply(constraints: List[Constraint]): Either[TypeError, List[Substitution]] =
    Time("unifier")(unifyAll(constraints).map(_.toList.sorted))

  def unifyAll(constraints: List[Constraint]): Step[Set[Substitution]] =
    constraints match {
      case Nil =>
        pure(Set.empty[Substitution])

      case head :: tail0 =>
        for {
          headSubst <- unifyOne(head)
          tail1      = substAll(tail0, headSubst)
          tailSubst <- unifyAll(tail1)
        } yield compose(headSubst, tailSubst)
    }

  def unifyOne(constraint: Constraint): Step[Set[Substitution]] =
    constraint match {
      case (a: ConcreteType) === (b: ConcreteType) if a == b =>
        pure(Set.empty[Substitution])

      case FuncType(aArgs, aRet) === FuncType(bArgs, bRet) =>
        unifyAll((aRet === bRet) :: (aArgs zip bArgs).map { case (a, b) => a === b })

      case ArrType(aItem) === ArrType(bItem) =>
        unifyOne(aItem === bItem)

      case (a: TypeVar) === b =>
        unifyVar(a, b)

      case a === (b: TypeVar) =>
        unifyVar(b, a)

      case a === b =>
        fail(TypeMismatch(a, b))
    }

  def unifyVar(a: TypeVar, b: Type): Step[Set[Substitution]] =
    if(a == b) {
      pure(Set.empty[Substitution])
    } else if(contains(a, b)) {
      fail(CyclicReference(a, b))
    } else {
      pure(Set(a --> b))
    }

  def substAll(constraints: List[Constraint], substs: Set[Substitution]): List[Constraint] =
    substs.foldLeft(constraints)((c, s) => c.map(substOne(_, s)))

  def substOne(constraint: Constraint, subst: Substitution): Constraint = {
    val l === r = constraint
    val a --> b = subst
    substVar(l, a, b) === substVar(r, a, b)
  }

  def substVar(tpe: Type, a: TypeVar, b: Type): Type =
    tpe match {
      case `a`                 => b
      case tpe: TypeVar        => tpe
      case tpe: TypeRef        => tpe
      case FuncType(args, ret) => FuncType(args.map(substVar(_, a, b)), substVar(ret, a, b))
      // case UnionType(types)    => UnionTypes(types.map(substVar(_, a, b)))
      // case ObjType()           => ???
      case ArrType(tpe)        => ArrType(substVar(tpe, a, b))
      case tpe @ StrType       => tpe
      case tpe @ IntType       => tpe
      case tpe @ DblType       => tpe
      case tpe @ NullType      => tpe
      case tpe @ BoolType      => tpe
      case tpe @ AnyType       => tpe
    }

  def compose(substs1: Set[Substitution], substs2: Set[Substitution]): Set[Substitution] =
    substs2 ++ substs1.map { subst1 =>
      val a --> b = subst1
      a --> substs2.foldLeft(b)((tpe, subst2) => substVar(tpe, subst2.lhs, subst2.rhs))
    }

  def contains(a: TypeVar, b: Type): Boolean =
    b match {
      case b: TypeVar          => a == b
      case b: TypeRef          => false
      case FuncType(args, ret) => args.exists(contains(a, _)) || contains(a, ret)
      // case UnionType(types)    => types.exists(contains(a))
      // case ObjType()           => ???
      case ArrType(b)          => a == b
      case StrType             => false
      case IntType             => false
      case DblType             => false
      case NullType            => false
      case BoolType            => false
      case AnyType             => false
    }

  def pure[A](value: A): Step[A] =
    Right(value)

  def fail[A](error: TypeError): Step[A] =
    Left(error)
}
