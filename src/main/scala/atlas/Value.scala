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

sealed abstract class FuncValue(val arity: Int) extends Value

final case class BoundFunc(func: FuncLiteral, env: Env) extends FuncValue(func.args.length)

final case class NativeFunc(override val arity: Int, func: List[Value] => Either[String, Value]) extends FuncValue(arity) {
  def orElse(that: NativeFunc): NativeFunc =
    if(this.arity == that.arity) {
      NativeFunc(this.arity, values => this.func(values).fold(_ => that.func(values), Right.apply))
    } else {
      ???
    }
}

object NativeFunc extends NativeFuncBoilerplate
