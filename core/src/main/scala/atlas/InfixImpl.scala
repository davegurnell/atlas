package atlas

trait InfixImpl[F[_]] {
  self: Interpreter[F] =>

  private val add: Native[F] =
    native((a: Int, b: Int) => a + b) orElse
    native((a: Double, b: Double) => a + b) orElse
    native((a: String, b: String) => a + b)

  private val sub: Native[F] =
    native((a: Int, b: Int) => a - b) orElse
    native((a: Double, b: Double) => a - b)

  private val mul: Native[F] =
    native((a: Int, b: Int) => a * b) orElse
    native((a: Double, b: Double) => a * b)

  private val div: Native[F] =
    native((a: Int, b: Int) => 1.0 * a / b) orElse
    native((a: Double, b: Double) => a / b)

  private val and: Native[F] =
    native((a: Boolean, b: Boolean) => a && b)

  private val or: Native[F] =
    native((a: Boolean, b: Boolean) => a || b)

  private val eq: Native[F] =
    native((a: Value[F], b: Value[F]) => a == b)

  private val ne: Native[F] =
    native((a: Value[F], b: Value[F]) => a != b)

  private val gt: Native[F] =
    native((a: Int, b: Int) => a > b) orElse
    native((a: Double, b: Double) => a > b) orElse
    native((a: String, b: String) => a > b) orElse
    native((a: Boolean, b: Boolean) => a > b)

  private val lt: Native[F] =
    native((a: Int, b: Int) => a < b) orElse
    native((a: Double, b: Double) => a < b) orElse
    native((a: String, b: String) => a < b) orElse
    native((a: Boolean, b: Boolean) => a < b)

  private val gte: Native[F] =
    native((a: Int, b: Int) => a >= b) orElse
    native((a: Double, b: Double) => a >= b) orElse
    native((a: String, b: String) => a >= b) orElse
    native((a: Boolean, b: Boolean) => a >= b)

  private val lte: Native[F] =
    native((a: Int, b: Int) => a <= b) orElse
    native((a: Double, b: Double) => a <= b) orElse
    native((a: String, b: String) => a <= b) orElse
    native((a: Boolean, b: Boolean) => a <= b)

  private val pos: Native[F] =
    native((a: Int) => a) orElse
    native((a: Double) => a)

  private val neg: Native[F] =
    native((a: Int) => -a) orElse
    native((a: Double) => -a)

  private val not: Native[F] =
    native((a: Boolean) => !a)

  def infixBindings: List[(String, Native[F])] =
    List(
      "infix:+"  -> add,
      "infix:-"  -> sub,
      "infix:*"  -> mul,
      "infix:/"  -> div,
      "infix:&&" -> and,
      "infix:||" -> or,
      "infix:==" -> eq,
      "infix:!=" -> ne,
      "infix:>"  -> gt,
      "infix:<"  -> lt,
      "infix:>=" -> gte,
      "infix:<=" -> lte,
    )
}
