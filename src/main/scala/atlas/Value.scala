package atlas

import cats.implicits._

sealed abstract class Value extends Product with Serializable

final case class ObjVal(fields: List[(String, Value)]) extends Value
final case class ArrVal(items: List[Value]) extends Value
final case class StrVal(value: String) extends Value
final case class IntVal(value: Int) extends Value
final case class DblVal(value: Double) extends Value
final case class BoolVal(value: Boolean) extends Value
case object NullVal extends Value

sealed abstract class FuncVal extends Value

final case class Closure(func: FuncExpr, env: Env) extends FuncVal {
  override def toString: String = s"Closure($func, ${env.chain.scopes.length})"
}

sealed abstract class NativeOp(val id: String) extends FuncVal

sealed abstract class InfixOp(override val id: String) extends NativeOp(id)

object InfixOp {
  case object Add extends InfixOp("+")
  case object Sub extends InfixOp("-")
  case object Mul extends InfixOp("*")
  case object Div extends InfixOp("/")
  case object And extends InfixOp("&&")
  case object Or extends InfixOp("||")
  case object Eq extends InfixOp("==")
  case object Ne extends InfixOp("!=")
  case object Gt extends InfixOp(">")
  case object Lt extends InfixOp("<")
  case object Gte extends InfixOp(">=")
  case object Lte extends InfixOp("<=")
}

sealed abstract class PrefixOp(override val id: String) extends NativeOp(id)

object PrefixOp {
  case object Not extends PrefixOp("!")
  case object Pos extends PrefixOp("+")
  case object Neg extends PrefixOp("-")
}

final case class Native(override val id: String) extends NativeOp(id)