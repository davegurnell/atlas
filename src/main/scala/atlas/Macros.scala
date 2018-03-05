package atlas

import fastparse.all._

import scala.reflect.macros.blackbox.Context

class Macros(val c: Context) {
  import c.universe.{Expr => _, _}

  def exprMacro(args: c.Tree *): c.Tree =
    macroImpl[Ast.Expr](Parser.parsers.exprToEnd, args, toTree)

  def stmtMacro(args: c.Tree *): c.Tree =
    macroImpl[Ast.Stmt](Parser.parsers.stmtToEnd, args, toTree)

  def progMacro(args: c.Tree *): c.Tree =
    macroImpl[Ast.Prog](Parser.parsers.progToEnd, args, toTree)

  private def macroImpl[A](parser: Parser[A], args: Seq[c.Tree], toTree: A => c.Tree): c.Tree = {
    if(args.nonEmpty) {
      c.abort(c.enclosingPosition, "Cannot interpolate in an expression literal")
    } else {
      c.prefix.tree match {
        case q"$_($_(..$partTrees))" =>
          parser.parse(formatCode(partTrees)) match {
            case Parsed.Success(value, _) =>
              toTree(value)

            case failure: Parsed.Failure =>
              c.abort(c.enclosingPosition, s"Error parsing atlas code: ${failure.toString}")
          }
      }
    }
  }

  // Adapted from UnindentMacros.transform(...) in davegurnell/unindent:
  private def formatCode(partTrees: Seq[c.Tree]): String = {
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

  private def toTree(stmt: Ast.Stmt): c.Tree =
    stmt match {
      case Ast.Defn(ref, expr)               => q"_root_.atlas.Ast.Defn(${toTree(ref)}, ${toTree(expr)})"
      case Ast.Ref(id)                       => q"_root_.atlas.Ast.Ref($id)"
      case Ast.Block(stmts, expr)            => q"_root_.atlas.Ast.Block(${stmts.map(toTree)}, ${toTree(expr)})"
      case Ast.Select(expr, ref)             => q"_root_.atlas.Ast.Select(${toTree(expr)}, ${toTree(ref)})"
      case Ast.Cond(test, trueArm, falseArm) => q"_root_.atlas.Ast.Cond(${toTree(test)}, ${toTree(trueArm)}, ${toTree(falseArm)})"
      case Ast.Infix(op, arg1, arg2)         => q"_root_.atlas.Ast.Infix(${toTree(op)}, ${toTree(arg1)}, ${toTree(arg2)})"
      case Ast.Prefix(op, arg)               => q"_root_.atlas.Ast.Prefix(${toTree(op)}, ${toTree(arg)})"
      case Ast.Apply(func, args)             => q"_root_.atlas.Ast.Apply(${toTree(func)}, ${args.map(toTree)})"
      case Ast.FuncLiteral(args, body)       => q"_root_.atlas.Ast.FuncLiteral(${args.map(toTree)}, ${toTree(body)})"
      case Ast.ObjectLiteral(fields)         => q"_root_.atlas.Ast.ObjectLiteral(${fields.map(toTree)})"
      case Ast.ArrayLiteral(items)           => q"_root_.atlas.Ast.ArrayLiteral(${items.map(toTree)})"
      case Ast.StringLiteral(value)          => q"_root_.atlas.Ast.StringLiteral($value)"
      case Ast.IntLiteral(value)             => q"_root_.atlas.Ast.IntLiteral($value)"
      case Ast.DoubleLiteral(value)          => q"_root_.atlas.Ast.DoubleLiteral($value)"
      case Ast.NullLiteral                   => q"_root_.atlas.Ast.NullLiteral"
      case Ast.TrueLiteral                   => q"_root_.atlas.Ast.TrueLiteral"
      case Ast.FalseLiteral                  => q"_root_.atlas.Ast.FalseLiteral"
    }

  private def toTree(op: PrefixOp): c.Tree =
    op match {
      case PrefixOp.Not => q"_root_.atlas.PrefixOp.Not"
      case PrefixOp.Pos => q"_root_.atlas.PrefixOp.Pos"
      case PrefixOp.Neg => q"_root_.atlas.PrefixOp.Neg"
    }


  private def toTree(op: InfixOp): c.Tree =
    op match {
      case InfixOp.Add => q"_root_.atlas.InfixOp.Add"
      case InfixOp.Sub => q"_root_.atlas.InfixOp.Sub"
      case InfixOp.Mul => q"_root_.atlas.InfixOp.Mul"
      case InfixOp.Div => q"_root_.atlas.InfixOp.Div"
      case InfixOp.And => q"_root_.atlas.InfixOp.And"
      case InfixOp.Or  => q"_root_.atlas.InfixOp.Or"
      case InfixOp.Eq  => q"_root_.atlas.InfixOp.Eq"
      case InfixOp.Ne  => q"_root_.atlas.InfixOp.Ne"
      case InfixOp.Gt  => q"_root_.atlas.InfixOp.Gt"
      case InfixOp.Lt  => q"_root_.atlas.InfixOp.Lt"
      case InfixOp.Gte => q"_root_.atlas.InfixOp.Gte"
      case InfixOp.Lte => q"_root_.atlas.InfixOp.Lte"
    }

  private def toTree(field: (String, Ast.Expr)): c.Tree = {
    val (name, expr) = field
    q"($name, ${toTree(expr)})"
  }

  private def toTree(prog: Ast.Prog): c.Tree =
    prog match {
      case Ast.Prog(stmts) => q"_root_.atlas.Ast.TopLevel(${stmts.map(toTree)})"
    }

}
