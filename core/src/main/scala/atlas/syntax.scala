package atlas

object syntax {
  implicit class EnvOps[F[_]](val chain: ScopeChain[String, Value[F]]) extends AnyVal {
    def set[A](id: String, value: A)(implicit enc: ValueEncoder[F, A]): Env[F] =
      chain.set(id, enc(value))

    def destructiveSet[A](id: String, value: A)(implicit enc: ValueEncoder[F, A]): Unit =
      chain.destructiveSet(id, enc(value))

    def destructiveSetAll[A](bindings: Seq[(String, A)])(implicit enc: ValueEncoder[F, A]): Unit =
      chain.destructiveSetAll(bindings.map { case (n, a) => (n, enc(a)) })
  }

  implicit class ValueEncoderOps[A](val a: A) extends AnyVal {
    def toAtlas[F[_]](implicit enc: ValueEncoder[F, A]): Value[F] =
      enc(a)
  }

  implicit class ValueDecoderOps[F[_]](val value: Value[F]) extends AnyVal {
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
