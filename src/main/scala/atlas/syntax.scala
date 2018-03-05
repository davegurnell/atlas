package atlas

object syntax {
  implicit class ValueEncoderOps[A](a: A) {
    def toValue(implicit enc: ValueEncoder[A]): Value =
      enc(a)
  }

  implicit class ValueDecoderOps(value: Value) {
    def as[A](implicit dec: ValueDecoder[A]): Either[String, A] =
      dec(value)
  }

  implicit class AtlasStringOps(val ctx: StringContext) extends AnyVal {
    def expr(args: Any *): Ast.Expr =
      macro Macros.exprMacro

    def stmt(args: Any *): Ast.Expr =
      macro Macros.stmtMacro

    def prog(args: Any *): Ast.Expr =
      macro Macros.progMacro
  }
}
