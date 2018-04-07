package atlas

import cats.MonadError
import cats.data.StateT
import cats.implicits._

sealed abstract class Value[F[_]] extends Product with Serializable

final case class ObjVal[F[_]](fields: List[(String, Value[F])]) extends Value[F]
final case class ArrVal[F[_]](items: List[Value[F]]) extends Value[F]
final case class StrVal[F[_]](value: String) extends Value[F]
final case class IntVal[F[_]](value: Int) extends Value[F]
final case class DblVal[F[_]](value: Double) extends Value[F]
final case class BoolVal[F[_]](value: Boolean) extends Value[F]
final case class NullVal[F[_]]() extends Value[F]

sealed abstract class FuncVal[F[_]] extends Value[F]

final case class Closure[F[_]](func: FuncExpr, env: Env[F]) extends FuncVal[F] {
  override def toString: String = s"Closure($func, ${env.scopes.length})"
}

final case class Native[F[_]](func: List[Value[F]] => EvalStep[F, Value[F]]) extends FuncVal[F] {
  def apply(args: List[Value[F]]): EvalStep[F, Value[F]] =
    func(args)

  def orElse(that: Native[F])(implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Native(args => this(args).recoverWith { case error => that(args) })
}
