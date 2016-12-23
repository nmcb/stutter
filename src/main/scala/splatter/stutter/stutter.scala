package splatter.stutter

trait Expr
case class Atom(value: String) extends Expr {
  override def toString: String = value
}
case class Lisp(expressions: Seq[Expr]) extends Expr {
  def isEmpty: Boolean = expressions == Nil
  override def toString: String = expressions.mkString("(", " ", ")")
}

object Stutter {
  val EmptyLisp = Lisp(Nil)
  val t = Atom("t")
  val f = EmptyLisp

  object Axioms {
    val QuoteOperator = Atom("quote")
    val AtomOperator  = Atom("atom")

    abstract class ArgumentExtractor(val operator: Atom) { self: Expr =>
      def apply(arg: Expr) = Lisp(Seq(operator, arg))
      def unapply(expr: Expr): Option[Expr] = expr match {
        case Lisp(Seq(`operator`, arg)) => Some(arg)
        case _                          => None
      }
    }

    object QuoteExpr extends ArgumentExtractor(QuoteOperator) with Expr
    object AtomExpr  extends ArgumentExtractor(AtomOperator)  with Expr

    def yields(e: Expr): Expr = e match {
      // quote expressions
      case QuoteExpr(arg) => arg
      // atom expressions
      case AtomExpr(arg)  => arg match {
        case _: Atom              => t
        case l: Lisp if l.isEmpty => t
        case QuoteExpr(a)         => a match {
          case _: Atom              => t
          case l: Lisp if l.isEmpty => t
          case _                    => f
        }
        case _: Lisp => yields(arg)
        case _       => f
      }
      case _  => sys.error("doesn't have an operator: " + e)
    }
  }

  object Parser {
    import fastparse.all._
    import fastparse.core.Parsed.{Failure, Success}
    import fastparse.all.parserApi


    val WhiteSpace: Seq[Char] = Seq('\r', '\n', '\t', '\f', '\b', ' ')
    val Characters: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z')

    val noop: P[Unit] = P(CharIn(WhiteSpace).?)
    val char: P[Unit] = P(CharIn(Characters))

    val atom: P[Atom] = P(char.rep(1).!.map(Atom))
    val list: P[Lisp] = P("(" ~ expr.rep.map(Lisp) ~ ")")
    val quot: P[Lisp] = P("'" ~ expr.map(e => Axioms.QuoteExpr(e)))
    val expr: P[Expr] = P(noop ~ (atom | list | quot) ~ noop)

    def parse(s: String): Expr = expr.parse(s) match {
      case Success(e, _)    => e
      case f: Failure[_, _] => sys.error(f.msg)
    }
  }
}
