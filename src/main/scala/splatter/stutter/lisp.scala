package splatter.stutter

trait Expr
case class Atom(s: String)    extends Expr
case class Lisp(e: Seq[Expr]) extends Expr
