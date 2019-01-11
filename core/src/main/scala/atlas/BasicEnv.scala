package atlas

import cats.implicits._

object BasicEnv {
  private val map: Value =
    new Native2[Native1[Value, Value], List[Value], List[Value]] {
      def apply[F[_]](func: Native1[Value, Value], list: List[Value])(implicit interpreter: Interpreter[F]): F[List[Value]] = {
        import interpreter._
        list.traverse(func(_))
      }
    }

  private val flatMap: Value =
    new Native2[Native1[Value, List[Value]], List[Value], List[Value]] {
      def apply[F[_]](func: Native1[Value, List[Value]], list: List[Value])(implicit interpreter: Interpreter[F]): F[List[Value]] = {
        import interpreter._
        list.flatTraverse(func(_))
      }
    }

  private val filter: Value =
    new Native2[Native1[Value, Boolean], List[Value], List[Value]] {
      def apply[F[_]](func: Native1[Value, Boolean], list: List[Value])(implicit interpreter: Interpreter[F]): F[List[Value]] = {
        import interpreter._
        list
          .traverse(value => func(value).map(test => if(test) Some(value) else None))
          .map(_.flatten)
      }
    }

  private val flatten: Value =
    new Native1[List[List[Value]], List[Value]] {
      def apply[F[_]](lists: List[List[Value]])(implicit interpreter: Interpreter[F]): F[List[Value]] = {
        import interpreter._
        lists.flatten.pure[F]
      }
    }

  def basicEnv: Env =
    Env.create
      .set("map",     map)
      .set("flatMap", flatMap)
      .set("filter",  filter)
      .set("flatten", flatten)
}
