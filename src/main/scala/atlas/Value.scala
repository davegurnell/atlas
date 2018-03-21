package atlas

import atlas.Ast._

sealed abstract class Value extends Product with Serializable

object Value extends NativeBoilerplate {
  sealed abstract class Data extends Value
  final case class Obj(fields: List[(String, Value)]) extends Data
  final case class Arr(items: List[Value]) extends Data
  final case class Str(value: String) extends Data
  final case class Intr(value: Int) extends Data
  final case class Real(value: Double) extends Data
  case object Null extends Data
  sealed abstract class Bool extends Data
  case object True extends Bool
  case object False extends Bool

  sealed abstract class Func extends Value
  final case class Closure(func: Literal.Func, env: Env) extends Func {
    override def toString: String = s"Closure($func, ${env.scopes.length})"
  }

  final case class Native(func: List[Value] => Either[Interpreter.Error, Value]) extends Func {
    def orElse(that: Native): Native =
      Native(values => this.func(values).fold(_ => that.func(values), Right.apply))
  }
}
