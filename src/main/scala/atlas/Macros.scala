package atlas

import fastparse.all._

import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {
  import c.universe.{Expr => _, Type => _, _}

  def exprMacro(args: Tree *): Tree =
    macroImpl[Ast.Expr](Parser.parsers.exprToEnd, args)

  def stmtMacro(args: Tree *): Tree =
    macroImpl[Ast.Stmt](Parser.parsers.stmtToEnd, args)

  def progMacro(args: Tree *): Tree =
    macroImpl[Ast.Expr](Parser.parsers.progToEnd, args)

  private def macroImpl[A](parser: Parser[A], args: Seq[Tree])(implicit lift: Liftable[A]): Tree = {
    if(args.nonEmpty) {
      c.abort(c.enclosingPosition, "Cannot interpolate in an expression literal")
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

  implicit def listLiftable[A](implicit itemLiftable: Liftable[A]): Liftable[List[A]] =
    new Liftable[List[A]] {
      def apply(list: List[A]): Tree =
        q"_root_.scala.List(..${list.map(itemLiftable.apply)})"
    }

  implicit lazy val stmtLiftable: Liftable[Ast.Stmt] =
    new Liftable[Ast.Stmt] {
      def apply(stmt: Ast.Stmt): Tree =
        stmt match {
          case Ast.DefnStmt(name, tpe, expr) => q"$pkg.Ast.DefnStmt($name, $tpe, $expr)"
          case Ast.ExprStmt(expr)            => q"$pkg.Ast.ExprStmt($expr)"
        }
    }

  implicit lazy val argLiftable: Liftable[Ast.Literal.Arg] =
    new Liftable[Ast.Literal.Arg] {
      def apply(arg: Ast.Literal.Arg): Tree =
        q"$pkg.Ast.Literal.Arg(${arg.name}, ${arg.tpe})"
    }

  implicit lazy val exprLiftable: Liftable[Ast.Expr] =
    new Liftable[Ast.Expr] {
      def apply(expr: Ast.Expr): Tree =
        expr match {
          case Ast.Ref(id)                  => q"$pkg.Ast.Ref($id)"
          case Ast.Block(stmts, expr)       => q"$pkg.Ast.Block($stmts, $expr)"
          case Ast.Select(expr, ref)        => q"$pkg.Ast.Select($expr, $ref)"
          case Ast.Cond(test, arm1, arm2)   => q"$pkg.Ast.Cond($test, $arm1, $arm2)"
          case Ast.Infix(op, arg1, arg2)    => q"$pkg.Ast.Infix($op, $arg1, $arg2)"
          case Ast.Prefix(op, arg)          => q"$pkg.Ast.Prefix($op, $arg)"
          case Ast.Cast(expr, asType)       => q"$pkg.Ast.Cast($expr, $asType)"
          case Ast.Apply(func, args)        => q"$pkg.Ast.Apply($func, $args)"
          case Ast.Literal.Func(args, body) => q"$pkg.Ast.Literal.Func($args, $body)"
          case Ast.Literal.Obj(fields)      => q"$pkg.Ast.Literal.Obj($fields)"
          case Ast.Literal.Arr(items)       => q"$pkg.Ast.Literal.Arr($items)"
          case Ast.Literal.Str(value)       => q"$pkg.Ast.Literal.Str($value)"
          case Ast.Literal.Intr(value)      => q"$pkg.Ast.Literal.Intr($value)"
          case Ast.Literal.Real(value)      => q"$pkg.Ast.Literal.Real($value)"
          case Ast.Literal.Null             => q"$pkg.Ast.Literal.Null"
          case Ast.Literal.True             => q"$pkg.Ast.Literal.True"
          case Ast.Literal.False            => q"$pkg.Ast.Literal.False"
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

  implicit lazy val typeLiftable: Liftable[Type] =
    new Liftable[Type] {
      def apply(tpe: Type): Tree =
        tpe match {
          case Type.Var(id)         => q"$pkg.Type.Var($id)"
          case Type.Ref(id)         => q"$pkg.Type.Ref($id)"
          case Type.Func(args, res) => q"$pkg.Type.Func($args, $res)"
          case Type.Union(types)    => q"$pkg.Type.Union($types)"
          case Type.Obj(fieldTypes) => q"$pkg.Type.Obj($fieldTypes)"
          case Type.Arr(tpe)        => q"$pkg.Type.Arr($tpe)"
          case Type.Str             => q"$pkg.Type.Str"
          case Type.Intr            => q"$pkg.Type.Intr"
          case Type.Real            => q"$pkg.Type.Real"
          case Type.Null            => q"$pkg.Type.Null"
          case Type.Bool            => q"$pkg.Type.Bool"
          case Type.Bottom          => q"$pkg.Type.Bottom"
          case Type.Top             => q"$pkg.Type.Top"
        }
    }

  implicit lazy val fieldPairLiftable: Liftable[(String, Ast.Expr)] =
    new Liftable[(String, Ast.Expr)] {
      def apply(field: (String, Ast.Expr)): Tree = {
        val (name, expr) = field
        q"($name, $expr)"
      }
    }

  implicit lazy val argPairLiftable: Liftable[(String, Type)] =
    new Liftable[(String, Type)] {
      def apply(field: (String, Type)): Tree = {
        val (name, tpe) = field
        q"($name, $tpe)"
      }
    }
}
