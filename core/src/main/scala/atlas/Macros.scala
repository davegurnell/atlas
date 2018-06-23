package atlas

import fastparse.all._

import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {
  import c.universe._

  def exprMacro(args: Tree *): Tree =
    macroImpl[ExprStx](Parser.parsers.exprToEnd, args)

  def progMacro(args: Tree *): Tree =
    macroImpl[ExprStx](Parser.parsers.progToEnd, args)

  private def macroImpl[A](parser: Parser[A], args: Seq[Tree])(implicit lift: Liftable[A]): Tree = {
    if(args.nonEmpty) {
      c.abort(c.enclosingPosition, "String interpolation not supported!")
    } else {
      c.prefix.tree match {
        case q"$_($_(..$partTrees))" =>
          parser.parse(formatCode(partTrees)) match {
            case Parsed.Success(value, _) =>
              lift(value)

            case failure: Parsed.Failure =>
              c.abort(c.enclosingPosition, s"Error parsing atlas code: $failure\n${failure.extra.traced}")
          }
      }
    }
  }

  // Adapted from UnindentMacros.transform(...) in davegurnell/unindent:
  private def formatCode(partTrees: Seq[Tree]): String = {
    import scala.util.matching.Regex.Match

    val prefixRegex = """^\n""".r
    val suffixRegex = """\n[ \t]*$""".r
    val indentRegex = """\n[ \t]+""".r

    val parts = partTrees.map { case Literal(Constant(part: String)) => part }

    val numParts = parts.length

    val minIndent = parts
      .flatMap(indentRegex.findAllIn)
      .map(_.length)
      .foldLeft(Int.MaxValue)(math.min)

    parts.zipWithIndex.map {
      case (part, index) =>
        // De-indent newlines:
        var ans = indentRegex.replaceAllIn(part, (m: Match) =>
          "\n" + (" " * (m.group(0).length - minIndent)))

        // Strip any initial newline from the beginning of the string:
        if(index == 0) {
          ans = prefixRegex.replaceFirstIn(ans, "")
        }

        // Strip any final newline from the end of the string:
        if(index == numParts - 1) {
          ans = suffixRegex.replaceFirstIn(ans, "")
        }

        ans
    }.mkString
  }

  val pkg = q"_root_.atlas"

  implicit lazy val exprLiftable: Liftable[ExprStx] =
    new Liftable[ExprStx] {
      def apply(expr: ExprStx): Tree =
        expr match {
          case RefExprStx(id)                        => q"$pkg.RefExprStx($id)"
          case AppExprStx(func, args)                => q"$pkg.AppExprStx($func, $args)"
          case InfixExprStx(op, arg1, arg2)          => q"$pkg.InfixExprStx($op, $arg1, $arg2)"
          case PrefixExprStx(op, arg)                => q"$pkg.PrefixExprStx($op, $arg)"
          case FuncExprStx(tArgs, args, rType, body) => q"$pkg.FuncExprStx($tArgs, $args, $rType, $body)"
          case BlockExprStx(stmts, expr)             => q"$pkg.BlockExprStx($stmts, $expr)"
          case SelectExprStx(expr, ref)              => q"$pkg.SelectExprStx($expr, $ref)"
          case CondExprStx(test, arm1, arm2)         => q"$pkg.CondExprStx($test, $arm1, $arm2)"
          case CastExprStx(expr, tpe)                => q"$pkg.CastExprStx($expr, $tpe)"
          case ParenExprStx(expr)                    => q"$pkg.ParenExprStx($expr)"
          case ObjExprStx(fields)                    => q"$pkg.ObjExprStx($fields)"
          case ArrExprStx(items)                     => q"$pkg.ArrExprStx($items)"
          case StrExprStx(value)                     => q"$pkg.StrExprStx($value)"
          case IntExprStx(value)                     => q"$pkg.IntExprStx($value)"
          case DblExprStx(value)                     => q"$pkg.DblExprStx($value)"
          case BoolExprStx(value)                    => q"$pkg.BoolExprStx($value)"
          case NullExprStx                           => q"$pkg.NullExprStx"
        }
    }

  implicit lazy val stmtLiftable: Liftable[StmtStx] =
    new Liftable[StmtStx] {
      def apply(stmt: StmtStx): Tree =
        stmt match {
          case ExprStmtStx(expr)           => q"$pkg.ExprStmtStx($expr)"
          case LetStmtStx(name, tpe, expr) => q"$pkg.LetStmtStx($name, $tpe, $expr)"
          case TypeStmtStx(name, tpe)      => q"$pkg.TypeStmtStx($name, $tpe)"
        }
    }

  implicit lazy val argLiftable: Liftable[FuncArgStx] =
    new Liftable[FuncArgStx] {
      def apply(arg: FuncArgStx): Tree =
        arg match {
          case FuncArgStx(name, tpe) => q"$pkg.FuncArgStx($name, $tpe)"
        }
    }

  implicit lazy val prefixOpLiftable: Liftable[PrefixOp] =
    new Liftable[PrefixOp] {
      def apply(op: PrefixOp): Tree =
        op match {
          case PrefixOp.Not => q"$pkg.PrefixOp.Not"
          case PrefixOp.Pos => q"$pkg.PrefixOp.Pos"
          case PrefixOp.Neg => q"$pkg.PrefixOp.Neg"
        }
    }

  implicit lazy val infixOpLiftable: Liftable[InfixOp] =
    new Liftable[InfixOp] {
      def apply(op: InfixOp): Tree =
        op match {
          case InfixOp.Add => q"$pkg.InfixOp.Add"
          case InfixOp.Sub => q"$pkg.InfixOp.Sub"
          case InfixOp.Mul => q"$pkg.InfixOp.Mul"
          case InfixOp.Div => q"$pkg.InfixOp.Div"
          case InfixOp.And => q"$pkg.InfixOp.And"
          case InfixOp.Or  => q"$pkg.InfixOp.Or"
          case InfixOp.Eq  => q"$pkg.InfixOp.Eq"
          case InfixOp.Ne  => q"$pkg.InfixOp.Ne"
          case InfixOp.Gt  => q"$pkg.InfixOp.Gt"
          case InfixOp.Lt  => q"$pkg.InfixOp.Lt"
          case InfixOp.Gte => q"$pkg.InfixOp.Gte"
          case InfixOp.Lte => q"$pkg.InfixOp.Lte"
        }
    }

  implicit lazy val typeLiftable: Liftable[TypeStx] =
    new Liftable[TypeStx] {
      def apply(tpe: TypeStx): Tree =
        tpe match {
          case RefTypeStx(name)             => q"$pkg.RefTypeStx($name)"
          case FuncTypeStx(vars, args, res) => q"$pkg.FuncTypeStx($vars, $args, $res)"
          case UnionTypeStx(a, b)           => q"$pkg.UnionTypeStx($a, $b)"
          case NullableTypeStx(tpe)         => q"$pkg.NullableTypeStx($tpe)"
          case ObjTypeStx(fields)           => q"$pkg.ObjTypeStx($fields)"
          case ArrTypeStx(item)             => q"$pkg.ArrTypeStx($item)"
          case ParenTypeStx(tpe)            => q"$pkg.ParenTypeStx($tpe)"
          case StrTypeStx                   => q"$pkg.StrTypeStx"
          case IntTypeStx                   => q"$pkg.IntTypeStx"
          case DblTypeStx                   => q"$pkg.DblTypeStx"
          case BoolTypeStx                  => q"$pkg.BoolTypeStx"
          case NullTypeStx                  => q"$pkg.NullTypeStx"
        }
    }

  implicit def listLiftable[A: Liftable]: Liftable[List[A]] =
    new Liftable[List[A]] {
      def apply(list: List[A]): Tree =
        q"_root_.scala.List(..$list)"
    }

  implicit def setLiftable[A: Liftable]: Liftable[Set[A]] =
    new Liftable[Set[A]] {
      def apply(list: Set[A]): Tree =
        q"_root_.scala.collection.immutable.Set(..$list)"
    }

  implicit def pairLiftable[A: Liftable]: Liftable[(String, A)] =
    new Liftable[(String, A)] {
      def apply(field: (String, A)): Tree = {
        val (name, expr) = field
        q"($name, $expr)"
      }
    }
}
