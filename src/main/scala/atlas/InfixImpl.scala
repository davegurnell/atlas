// package atlas

// object InfixImpl {
//   import Value._

//   val add: Native =
//     native((a: Int, b: Int) => a + b) orElse
//     native((a: Double, b: Double) => a + b) orElse
//     native((a: String, b: String) => a + b)

//   val sub: Native =
//     native((a: Int, b: Int) => a - b) orElse
//     native((a: Double, b: Double) => a - b)

//   val mul: Native =
//     native((a: Int, b: Int) => a * b) orElse
//     native((a: Double, b: Double) => a * b)

//   val div: Native =
//     native((a: Int, b: Int) => 1.0 * a / b) orElse
//     native((a: Double, b: Double) => a / b)

//   val and: Native =
//     native((a: Boolean, b: Boolean) => a && b)

//   val or: Native =
//     native((a: Boolean, b: Boolean) => a || b)

//   val eq: Native =
//     native((a: Value, b: Value) => a == b)

//   val ne: Native =
//     native((a: Value, b: Value) => a != b)

//   val gt: Native =
//     native((a: Int, b: Int) => a > b) orElse
//     native((a: Double, b: Double) => a > b) orElse
//     native((a: String, b: String) => a > b) orElse
//     native((a: Boolean, b: Boolean) => a > b)

//   val lt: Native =
//     native((a: Int, b: Int) => a < b) orElse
//     native((a: Double, b: Double) => a < b) orElse
//     native((a: String, b: String) => a < b) orElse
//     native((a: Boolean, b: Boolean) => a < b)

//   val gte: Native =
//     native((a: Int, b: Int) => a >= b) orElse
//     native((a: Double, b: Double) => a >= b) orElse
//     native((a: String, b: String) => a >= b) orElse
//     native((a: Boolean, b: Boolean) => a >= b)

//   val lte: Native =
//     native((a: Int, b: Int) => a <= b) orElse
//     native((a: Double, b: Double) => a <= b) orElse
//     native((a: String, b: String) => a <= b) orElse
//     native((a: Boolean, b: Boolean) => a <= b)

//   def apply(op: InfixOp): Native =
//     op match {
//       case InfixOp.Add => add
//       case InfixOp.Sub => sub
//       case InfixOp.Mul => mul
//       case InfixOp.Div => div
//       case InfixOp.And => and
//       case InfixOp.Or  => or
//       case InfixOp.Eq  => eq
//       case InfixOp.Ne  => ne
//       case InfixOp.Gt  => gt
//       case InfixOp.Lt  => lt
//       case InfixOp.Gte => gte
//       case InfixOp.Lte => lte
//     }
// }
