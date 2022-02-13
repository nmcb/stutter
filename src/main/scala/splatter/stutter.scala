package splatter
package stutter

object Stutter {

  sealed trait Expr

  case class Atom(value: String) extends Expr {
    override def toString: String =
      value
  }

  case class Lisp(expressions: Seq[Expr]) extends Expr {
    def isEmpty: Boolean =
      expressions.isEmpty

    override def toString: String =
      expressions.mkString("(", " ", ")")
  }

  val t = Atom("t")
  val f = Lisp(Nil)

  sealed abstract trait Extractable[T] extends Expr {
    def extract: PartialFunction[Expr,T]
    def unapply(e: Expr): Option[T] = extract.lift(e)
  }

  sealed abstract class Primitive[T](name: String) extends Extractable[T] {    
    val Op: Atom = Atom(name)
  }

  sealed abstract class Primitive1(name: String)
    extends Primitive[Expr](name) {
      def extract: PartialFunction[Expr,Expr] =
        { case Lisp(Seq(Op, arg)) => arg }
  }

  sealed abstract class Primitive2(name: String)
    extends Primitive[(Expr,Expr)](name) {
      def extract: PartialFunction[Expr,(Expr,Expr)] =
        { case Lisp(Seq(Op, a, b)) => (a, b) }
  }

  sealed abstract class PrimitiveN(name: String)
    extends Primitive[Seq[Expr]](name) {
      def extract: PartialFunction[Expr,Seq[Expr]] =
        { case Lisp(Op +: args) => args }
    }

  object Quote extends Primitive1("quote")
  object Atom  extends Primitive1("atom")
  object Eq    extends Primitive2("eq")
  object Car   extends Primitive1("car")
  object Cdr   extends Primitive1("cdr")
  object Cons  extends Primitive2("cons")
  object Cond  extends PrimitiveN("cond")

  val PrimitiveOps: Seq[Atom] =
    Seq(Atom.Op, Quote.Op, Eq.Op, Car.Op, Cdr.Op, Cons.Op, Cond.Op)

  object Lambda extends Primitive[(Seq[Expr],Expr)]("lambda") {
    def extract: PartialFunction[Expr,(Seq[Expr],Expr)] =
      { case Lisp(Seq(Lambda.Op, Lisp(parms), expr)) => (parms, expr) }
  }

  object Function extends Extractable[(Seq[Expr],Expr,Seq[Expr])] {
    def extract: PartialFunction[Expr,(Seq[Expr],Expr,Seq[Expr])] =
      { case Lisp(Lambda(parms, expr) +: args) => (parms, expr, args) }
  }

  def eval(s: String): Expr =
    eval(Parser.parseLisp(s))

  def eval(e: Expr): Expr = e match {
    // function calls first
    case Function(parms, expr, args) =>
      expr match {

        // parameters as operators.
        case Lisp((op : Atom) +: fargs) if !PrimitiveOps.contains(op) && args.nonEmpty && isQuotedLambda(args.head) =>
          // unquote the lambda so that it can be evaluated
          val Lisp(Seq(Quote.Op, lambda)) = args.head
          eval(Lisp(lambda +: fargs))

        // parameters as arguments.
        case l: Lisp => eval(replace(l, parms.zip(args.map({
          // omit quoted expressions during replacement evaluation
          case Lisp(Seq(Quote.Op, lambda)) => Lisp(Seq(Quote.Op, lambda))
          case e: Expr => eval(e)
        })).toMap))

        case _ => sys.error(s"eval $e")
      }

    // primitive operations last
    case Quote(arg) => arg
    case Atom(arg)  => eval(arg) match {
      case a: Atom              => t
      case l: Lisp if l.isEmpty => t
      case _                    => f
    }
    case Eq(a, b) => (eval(a), eval(b)) match {
      case (a1: Atom, a2: Atom) if a1 == a2                 => t
      case (l1: Lisp, l2: Lisp) if l1.isEmpty && l2.isEmpty => t
      case _                                                => f
    }
    case Car(arg) => eval(arg) match {
      case l: Lisp if l.isEmpty => sys.error("car on empty list")
      case l: Lisp              => l.expressions.head
      case _ => sys.error("not a list")
    }
    case Cdr(arg) => eval(arg) match {
      case l: Lisp if l.expressions.size <= 1 => sys.error("cdr on empty or singleton list")
      case l: Lisp                            => Lisp(l.expressions.tail)
      case _ => sys.error("not a list")
    }
    case Cons(a, b) => (eval(a), eval(b)) match {
      case (e, Lisp(es)) => Lisp(e +: es)
      case _             => sys.error("not a list")
    }
    case Cond(args) => {
      val Lisp(Seq(_, expr)) = args.find(l => l match {
          case Lisp(Seq(p, e)) => eval(p) == t
          case _ => sys.error(s"no list of sequence $l")
        }).getOrElse(sys.error("undefined"))

      eval(expr)
    }
    case _ => sys.error(s"invalid expression: $e")
  }

  def isQuotedLambda(expr: Expr): Boolean = expr match {
    case Lisp(Seq(Quote.Op, Lisp(Seq(Lambda.Op, Lisp(_), _)))) => true
    case _                            => false
  }

  def replace(l: Lisp, parms: Map[Expr, Expr]): Lisp = {
    Lisp(l.expressions.map({ // TODO clearly Lisp needs a `map`.
      case a: Atom if parms.keySet.contains(a) => parms(a)
      case a: Atom => a
      case r: Lisp => replace(r, parms)
      case e: Expr => sys.error(s"non lisp expression: $e")
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
        _ <- keyword(")")
      } yield l

    def quot: P[Lisp] =
      (keyword("'") | keyword("’")) ~ expr.map(e => Lisp(Seq(Quote.Op, e)))

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
