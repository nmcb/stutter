package splatter
package stutter

object Stutter {

  sealed trait Expr

  case class Atom(value: String) extends Expr {
    override def toString: String = value
  }

  case class Lisp(expressions: Seq[Expr]) extends Expr {
    def isEmpty: Boolean = expressions.isEmpty
    override def toString: String = expressions.mkString("(", " ", ")")
  }

  val t = Atom("t")
  val f = Lisp(Nil)

  val AtomOp   = Atom("atom")
  val QuoteOp  = Atom("quote")
  val EqOp     = Atom("eq")
  val CarOp    = Atom("car")
  val CdrOp    = Atom("cdr")
  val ConsOp   = Atom("cons")
  val CondOp   = Atom("cond")

  val PrimitiveOps = Seq(AtomOp, QuoteOp, EqOp, CarOp, CdrOp, ConsOp, CondOp)

  val LambdaOp = Atom("lambda")

  def eval(s: String): Expr =
    eval(Parser.parseLisp(s))

  def eval(e: Expr): Expr = e match {
    // function calls first
    case Lisp(Lisp(Seq(LambdaOp, Lisp(parms), expr)) +: args) =>
      expr match {

        // parameters as operators.
        case Lisp((op : Atom) +: fargs)
          if !PrimitiveOps.contains(op) && args.nonEmpty && isQuotedLambda(args.head) =>
            // unquote the lambda so that it can be evaluated
            val Lisp(Seq(QuoteOp, lambda)) = args.head
          eval(Lisp(lambda +: fargs))

        // parameters as arguments.
        case l: Lisp => eval(replace(l, parms.zip(args.map({
          // omit quoted expressions during replacement evaluation
          case Lisp(Seq(QuoteOp, lambda)) => Lisp(Seq(QuoteOp, lambda))
          case e: Expr => eval(e)
        })).toMap))

        case _ => sys.error(s"eval $e")
      }

    // primitive operations last
    case Lisp(Seq(QuoteOp, arg)) => arg
    case Lisp(Seq(AtomOp, arg)) => eval(arg) match {
      case a: Atom              => t
      case l: Lisp if l.isEmpty => t
      case _                    => f
    }
    case Lisp(Seq(EqOp, x, y))   => (eval(x), eval(y)) match {
      case (a1: Atom, a2: Atom) if a1 == a2                 => t
      case (l1: Lisp, l2: Lisp) if l1.isEmpty && l2.isEmpty => t
      case _                                                => f
    }
    case Lisp(Seq(CarOp, arg))  => eval(arg) match {
      case l: Lisp if l.isEmpty => sys.error("car on empty list")
      case l: Lisp              => l.expressions.head
      case _ => sys.error("not a list")
    }
    case Lisp(Seq(CdrOp, arg))       => eval(arg) match {
      case l: Lisp if l.expressions.size <= 1 => sys.error("cdr on empty or singleton list")
      case l: Lisp                            => Lisp(l.expressions.tail)
      case _ => sys.error("not a list")
    }
    case Lisp(Seq(ConsOp, x, y)) => (eval(x), eval(y)) match {
      case (e, Lisp(es)) => Lisp(e +: es)
      case _             => sys.error("not a list")
    }
    case Lisp(CondOp +: args)   => {
      val Lisp(Seq(_, expr)) = args.find(l => l match {
          case Lisp(Seq(p, e)) => eval(p) == t
          case _ => sys.error(s"no list of sequence $l")
        }).getOrElse(sys.error("undefined"))

      eval(expr)
    }
    case _ => sys.error("invalid expression: " + e)
  }

  def isQuotedLambda(expr: Expr): Boolean = expr match {
    case Lisp(Seq(QuoteOp, Lisp(Seq(`LambdaOp`, Lisp(_), _)))) => true
    case _                            => false
  }

  def replace(l: Lisp, parms: Map[Expr, Expr]): Lisp = {
    Lisp(l.expressions.map({ // TODO clearly Lisp needs a `map`.
      case a: Atom if parms.keySet.contains(a) => parms(a)
      case a: Atom => a
      case r: Lisp => replace(r, parms)
    }))
  }

  object Parser {
    
    import parsing._
    import P._

    def atom: P[Atom] =
      satisfy(c => c.isLetter).oneOrMore.map(cs => Atom(cs.mkString("")))

    def list: P[Lisp] =
      for {
        _ <- keyword("(")
        l <- expr.zeroOrMore.map(es => Lisp(es))
        _ <- spaces
        _ <- keyword(")")
      } yield l

    def quot: P[Lisp] =
      (keyword("'") | keyword("’")) ~ expr.map(e => Lisp(Seq(QuoteOp, e)))

    def expr: P[Expr] =
      for {
        _ <- spaces
        e <- (atom | list | quot)
        _ <- spaces
      } yield e

    def parseLisp(s: String): Expr =
      run(expr)(s)
  }
}
