package atlas

trait InfixImpl {
  private val add: Native =
    Native((a: Int, b: Int) => a + b) orElse
    Native((a: Double, b: Double) => a + b) orElse
    Native((a: String, b: String) => a + b)

  private val sub: Native =
    Native((a: Int, b: Int) => a - b) orElse
    Native((a: Double, b: Double) => a - b)

  private val mul: Native =
    Native((a: Int, b: Int) => a * b) orElse
    Native((a: Double, b: Double) => a * b)

  private val div: Native =
    Native((a: Int, b: Int) => 1.0 * a / b) orElse
    Native((a: Double, b: Double) => a / b)

  private val and: Native =
    Native((a: Boolean, b: Boolean) => a && b)

  private val or: Native =
    Native((a: Boolean, b: Boolean) => a || b)

  private val eq: Native =
    Native((a: Value, b: Value) => a == b)

  private val ne: Native =
    Native((a: Value, b: Value) => a != b)

  private val gt: Native =
    Native((a: Int, b: Int) => a > b) orElse
    Native((a: Double, b: Double) => a > b) orElse
    Native((a: String, b: String) => a > b) orElse
    Native((a: Boolean, b: Boolean) => a > b)

  private val lt: Native =
    Native((a: Int, b: Int) => a < b) orElse
    Native((a: Double, b: Double) => a < b) orElse
    Native((a: String, b: String) => a < b) orElse
    Native((a: Boolean, b: Boolean) => a < b)

  private val gte: Native =
    Native((a: Int, b: Int) => a >= b) orElse
    Native((a: Double, b: Double) => a >= b) orElse
    Native((a: String, b: String) => a >= b) orElse
    Native((a: Boolean, b: Boolean) => a >= b)

  private val lte: Native =
    Native((a: Int, b: Int) => a <= b) orElse
    Native((a: Double, b: Double) => a <= b) orElse
    Native((a: String, b: String) => a <= b) orElse
    Native((a: Boolean, b: Boolean) => a <= b)

  private val pos: Native =
    Native((a: Int) => a) orElse
    Native((a: Double) => a)

  private val neg: Native =
    Native((a: Int) => -a) orElse
    Native((a: Double) => -a)

  private val not: Native =
    Native((a: Boolean) => !a)

  def infixImpl(op: InfixOp): Native =
    op match {
      case InfixOp.Add  => add
      case InfixOp.Sub  => sub
      case InfixOp.Mul  => mul
      case InfixOp.Div  => div
      case InfixOp.And  => and
      case InfixOp.Or   => or
      case InfixOp.Eq   => eq
      case InfixOp.Ne   => ne
      case InfixOp.Gt   => gt
      case InfixOp.Lt   => lt
      case InfixOp.Gte  => gte
      case InfixOp.Lte  => lte
    }
}
