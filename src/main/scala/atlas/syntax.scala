package atlas

object syntax {
//   implicit class ValueEncoderOps[A](a: A) {
//     def toValue(implicit enc: ValueEncoder[A]): Value =
//       enc(a)
//   }

//   implicit class ValueDecoderOps(value: Value) {
//     def as[A](implicit dec: ValueDecoder[A]): Either[RuntimeError, A] =
//       dec(value)
//   }

  def v(id: Int): TypeVar =
    TypeVar(id)

  implicit class AtlasStringOps(val ctx: StringContext) extends AnyVal {
    def expr(args: Any *): Expr =
      macro Macros.exprMacro

    def prog(args: Any *): Expr =
      macro Macros.progMacro
  }

  implicit class ConstraintOps(lhs: Type) {
    def ===(rhs: Type): Constraint =
      Constraint(lhs, rhs)
  }

  object === {
    def unapply(constraint: Constraint): Option[(Type, Type)] =
      Constraint.unapply(constraint)
  }

  implicit class SubstitutionOps(lhs: TypeVar) {
    def -->(rhs: Type): Substitution =
      Substitution(lhs, rhs)
  }

  object --> {
    def unapply(subst: Substitution): Option[(TypeVar, Type)] =
      Substitution.unapply(subst)
  }
}
