package splatter
package stutter

import fastparse.all._
import fastparse.core.Parsed.{Failure, Success}

object Parser {
  val WhiteSpace: Seq[Char] = Seq('\r', '\n', '\t', '\f', '\b', ' ')
  val Characters: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z')

  val noop: P[Unit] = P( CharIn(WhiteSpace).? )
  val char: P[Unit] = P( CharIn(Characters)   )

  val atom: P[Atom] = P( noop ~ char.rep(0).!.map(Atom) ~ noop )
  val list: P[Lisp] = P( noop ~ "(" ~ noop ~ expr.rep(0) ~ ")" ~ noop).map(Lisp)
  val expr: P[Expr] = P( list | atom )

  def parse(s: String): Expr = expr.parse(s) match {
    case Success(e, _)   => e
    case f: Failure[_,_] => sys.error(f.msg)
  }
}
