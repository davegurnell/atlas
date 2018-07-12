package atlas

import cats._
import cats.implicits._

case class EvalStep[F[_], A](run: (EvalStep.State[F]) => F[(EvalStep.State[F], A)]) {
  def runA(initialState: EvalStep.State[F])(implicit functor: Functor[F]): F[A] =
    run(initialState).map(_._2)
}

object EvalStep {
  type State[F[_]] = (Env[F], Limits)

  implicit def monadError[F[_]](implicit monadError: MonadError[F, RuntimeError]): MonadError[EvalStep[F, ?], RuntimeError] =
    new MonadError[EvalStep[F, ?], RuntimeError] {
      def pure[A](a: A): EvalStep[F, A] =
        EvalStep(state => monadError.pure((state, a)))

      def flatMap[A, B](fa: EvalStep[F, A])(f: A => EvalStep[F,B]): EvalStep[F,B] =
        EvalStep(state => monadError.flatMap(fa.run(state)) { case (state, a) => f(a).run(state) })

      def tailRecM[A, B](a: A)(f: A => EvalStep[F,Either[A,B]]): EvalStep[F,B] =
        EvalStep(state => monadError.map(monadError.tailRecM(a)(a => f(a).runA(state)))(eab => (state, eab)))

      def handleErrorWith[A](fa: EvalStep[F, A])(f: RuntimeError => EvalStep[F, A]): EvalStep[F, A] =
        EvalStep(state => monadError.handleErrorWith(fa.run(state))(error => f(error).run(state)))

      def raiseError[A](e: RuntimeError): EvalStep[F, A] =
        EvalStep(state => monadError.map(monadError.raiseError(e))((a: A) => (state, a)))
    }

  def inspectF[F[_], A](func: State[F] => F[A])(implicit monad: Monad[F]): EvalStep[F, A] =
    EvalStep(state => monad.map(func(state))(a => (state, a)))

  def modifyF[F[_]](func: State[F] => F[State[F]])(implicit monad: Monad[F]): EvalStep[F, Unit] =
    EvalStep(state => monad.map(func(state))(state => (state, ())))

  def liftF[F[_], A](fa: F[A])(implicit monad: Monad[F]): EvalStep[F, A] =
    EvalStep(state => monad.map(fa)(a => (state, a)))
}