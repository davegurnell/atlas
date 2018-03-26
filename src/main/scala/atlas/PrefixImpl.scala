// package atlas

// object PrefixImpl {
//   import Value._

//   def pos: Native =
//     native((a: Int) => a) orElse
//     native((a: Double) => a)

//   def neg: Native =
//     native((a: Int) => -a) orElse
//     native((a: Double) => -a)

//   def not: Native =
//     native((a: Boolean) => !a)

//   def apply(op: PrefixOp): Native =
//     op match {
//       case PrefixOp.Pos => pos
//       case PrefixOp.Neg => neg
//       case PrefixOp.Not => not
//     }
// }
