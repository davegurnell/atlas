package atlas

object syntax {
  implicit class ValueEncoderOps[A](a: A) {
    def toAtlas(implicit enc: ValueEncoder[A]): Value =
      enc(a)
  }

  implicit class ValueDecoderOps(value: Value) {
    def toScala[A](implicit dec: ValueDecoder[A]): Either[RuntimeError, A] =
      dec(value)
  }

  implicit class AtlasStringOps(val ctx: StringContext) extends AnyVal {
    def expr(args: Any *): Expr =
      macro Macros.exprMacro

    def prog(args: Any *): Expr =
      macro Macros.progMacro
  }
}
