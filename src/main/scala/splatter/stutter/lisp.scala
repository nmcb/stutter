package splatter.stutter

trait Expr
case class Atom(value: String) extends Expr
case class Lisp(expressions: Seq[Expr]) extends Expr {
  def isEmpty: Boolean = expressions == Nil
}

object Expr {
  val EmptyLisp = Lisp(Nil)
  val t = Atom("t")
  val f = EmptyLisp

  object Axioms {
    val QuoteOperator = Atom("quote")
    val AtomOperator  = Atom("atom")

    abstract class SingleArgExtractor(val operator: Atom) { self: Expr =>
      def apply(arg: Expr) = Lisp(Seq(operator, arg))
      def unapply(expr: Expr): Option[Expr] = expr match {
        case Lisp(Seq(`operator`, arg)) => Some(arg)
        case _                          => None
      }
    }

    object QuoteExpr extends SingleArgExtractor(QuoteOperator) with Expr
    object AtomExpr  extends SingleArgExtractor(AtomOperator)  with Expr

    def yields(e: Expr): Expr = e match {
      case QuoteExpr(arg) => arg
      case AtomExpr(arg)  => arg match {
        case a: Atom              => Expr.t
        case l: Lisp if l.isEmpty => Expr.t
        case _                    => Expr.EmptyLisp
      }
      case _         => sys.error("doesn't have an operator: " + e)
    }
  }
}
