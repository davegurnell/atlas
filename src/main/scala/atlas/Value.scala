package atlas

import cats.implicits._

sealed abstract class Value extends Product with Serializable

// final case class ObjVal(fields: List[(String, Value)]) extends Value
// final case class ArrVal(items: List[Value]) extends Value
final case class StrVal(value: String) extends Value
final case class IntVal(value: Int) extends Value
final case class DblVal(value: Double) extends Value
final case class BoolVal(value: Boolean) extends Value
case object NullVal extends Value

sealed abstract class FuncVal extends Value
final case class Closure(func: FuncExpr, env: Env) extends FuncVal {
  override def toString: String = s"Closure($func, ${env.chain.scopes.length})"
}

// final case class Native(tpe: Type, run: List[Value] => Interpreter.Step[Value]) extends FuncVal {
//   def orElse(that: Native): Native =
//     Native(
//       UnionType(Set(this.tpe, that.tpe)),
//       values => {
//         this.run(values).leftFlatMap(_ => that.run(values))
//       }
//     )
// }
