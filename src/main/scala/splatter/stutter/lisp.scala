package splatter.stutter

trait Expr
case class Atom(str: String)    extends Expr
case class Lisp(exs: Seq[Expr]) extends Expr

object Expr {
  val t = Atom("t")
  val f = Lisp(Nil)

  val Quote = Atom("quote")

  def Quote(e: Expr): Lisp = Lisp(Seq(Quote, e))

  def yields(e: Expr): Expr = e match {
    case Lisp(exs) => exs match {
      case Quote :: e :: Nil => e
      case _                 => sys.error("doesn't yield a value: " + e)
    }
    case _         => sys.error("doesn't have an operator: " + e)
  }
}
