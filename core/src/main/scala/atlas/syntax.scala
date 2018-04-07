package atlas

object syntax {
  implicit class ValueEncoderOps[A](a: A) {
    def toAtlas[F[_]](implicit enc: ValueEncoder[F, A]): Value[F] =
      enc(a)
  }

  implicit class ValueDecoderOps[F[_]](value: Value[F]) {
    def toScala[A](implicit dec: ValueDecoder[F, A]): F[A] =
      dec(value)
  }

  implicit class AtlasStringOps(val ctx: StringContext) extends AnyVal {
    def expr(args: Any *): Expr =
      macro Macros.exprMacro

    def prog(args: Any *): Expr =
      macro Macros.progMacro
  }
}
