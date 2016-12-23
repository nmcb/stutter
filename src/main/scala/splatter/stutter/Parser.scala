package splatter
package stutter

import fastparse.all._
import fastparse.core.Parsed.{Failure, Success}

object Parser {
  val WhiteSpace: Seq[Char] = Seq('\r', '\n', '\t', '\f', '\b', ' ')
  val Characters: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z')

  val noop: P[Unit] = P(CharIn(WhiteSpace).?)
  val char: P[Unit] = P(CharIn(Characters))

  val atom: P[Atom] = P(char.rep(1).!.map(Atom))
  val list: P[Lisp] = P("(" ~ expr.rep.map(Lisp) ~ ")")
  val quot: P[Lisp] = P("'" ~ expr.map(e => Expr.Axioms.QuoteExpr(e)))
  val expr: P[Expr] = P(noop ~ (atom | list | quot) ~ noop)

  def parse(s: String): Expr = expr.parse(s) match {
    case Success(e, _)    => e
    case f: Failure[_, _] => sys.error(f.msg)
  }
}
