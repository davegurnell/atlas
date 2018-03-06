package atlas

import cats.implicits._
import atlas.Ast._

sealed abstract class Value extends Product with Serializable

sealed abstract class DataValue extends Value
final case class ObjectValue(fields: List[(String, Value)]) extends DataValue
final case class ArrayValue(items: List[Value]) extends DataValue
final case class StringValue(value: String) extends DataValue
final case class IntValue(value: Int) extends DataValue
final case class DoubleValue(value: Double) extends DataValue
case object NullValue extends DataValue
sealed abstract class BooleanValue extends DataValue
case object TrueValue extends BooleanValue
case object FalseValue extends BooleanValue

sealed abstract class FuncValue extends Value
final case class Closure(func: FuncLiteral, env: Env) extends FuncValue {
  override def toString: String =
    s"Closure($func, ${env.scopes.length})"
}

final case class NativeFunc(func: List[Value] => Either[String, Value]) extends FuncValue {
  def orElse(that: NativeFunc): NativeFunc =
    NativeFunc(values => this.func(values).fold(_ => that.func(values), Right.apply))
}

object NativeFunc extends NativeFuncBoilerplate
