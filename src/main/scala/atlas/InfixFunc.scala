package atlas

object InfixFunc {
  val add: NativeFunc =
    NativeFunc((a: Int, b: Int) => a + b) orElse
    NativeFunc((a: Double, b: Double) => a + b) orElse
    NativeFunc((a: String, b: String) => a + b)

  val sub: NativeFunc =
    NativeFunc((a: Int, b: Int) => a - b) orElse
    NativeFunc((a: Double, b: Double) => a - b)

  val mul: NativeFunc =
    NativeFunc((a: Int, b: Int) => a * b) orElse
    NativeFunc((a: Double, b: Double) => a * b)

  val div: NativeFunc =
    NativeFunc((a: Int, b: Int) => 1.0 * a / b) orElse
    NativeFunc((a: Double, b: Double) => a / b)

  val and: NativeFunc =
    NativeFunc((a: Boolean, b: Boolean) => a && b)

  val or: NativeFunc =
    NativeFunc((a: Boolean, b: Boolean) => a || b)

  val eq: NativeFunc =
    NativeFunc((a: Value, b: Value) => a == b)

  val ne: NativeFunc =
    NativeFunc((a: Value, b: Value) => a != b)

  val gt: NativeFunc =
    NativeFunc((a: Int, b: Int) => a > b) orElse
    NativeFunc((a: Double, b: Double) => a > b) orElse
    NativeFunc((a: String, b: String) => a > b) orElse
    NativeFunc((a: Boolean, b: Boolean) => a > b)

  val lt: NativeFunc =
    NativeFunc((a: Int, b: Int) => a < b) orElse
    NativeFunc((a: Double, b: Double) => a < b) orElse
    NativeFunc((a: String, b: String) => a < b) orElse
    NativeFunc((a: Boolean, b: Boolean) => a < b)

  val gte: NativeFunc =
    NativeFunc((a: Int, b: Int) => a >= b) orElse
    NativeFunc((a: Double, b: Double) => a >= b) orElse
    NativeFunc((a: String, b: String) => a >= b) orElse
    NativeFunc((a: Boolean, b: Boolean) => a >= b)

  val lte: NativeFunc =
    NativeFunc((a: Int, b: Int) => a <= b) orElse
    NativeFunc((a: Double, b: Double) => a <= b) orElse
    NativeFunc((a: String, b: String) => a <= b) orElse
    NativeFunc((a: Boolean, b: Boolean) => a <= b)

  def apply(op: InfixOp): NativeFunc =
    op match {
      case InfixOp.Add => add
      case InfixOp.Sub => sub
      case InfixOp.Mul => mul
      case InfixOp.Div => div
      case InfixOp.And => and
      case InfixOp.Or  => or
      case InfixOp.Eq  => eq
      case InfixOp.Ne  => ne
      case InfixOp.Gt  => gt
      case InfixOp.Lt  => lt
      case InfixOp.Gte => gte
      case InfixOp.Lte => lte
    }
}
