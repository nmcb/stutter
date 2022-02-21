package splatter
package stutter

object Stutter {

  sealed trait Expr {
    def isAtom: Boolean
    def isLisp: Boolean
    def subs: Seq[Expr]
  }

  case class Atom(value: String) extends Expr {
    def isAtom: Boolean = true
    def isLisp: Boolean = false
    def subs: Seq[Expr] = Nil
    override def toString: String = value
  }

  case class Lisp(subs: Seq[Expr]) extends Expr {
    def isAtom: Boolean  = false
    def isLisp: Boolean  = true
    def isEmpty: Boolean = subs.isEmpty

    override def toString: String = subs.mkString("(", " ", ")")
  }

  // wtf ffs - special values?! - relax.. these were the 1960s - legacy
  val t = Atom("t")
  val f = Lisp(Nil)

  sealed trait Extractable[T] {
    def extract: PartialFunction[Expr,T]

    def unapply(e: Expr): Option[T] = extract.lift(e)
    def is(e: Expr): Boolean        = unapply(e).isDefined
  }

  sealed abstract class Primitive[T](name: String) extends Extractable[T] {    
    val Op: Atom = Atom(name)
  }

  sealed abstract class Primitive1(name: String) extends Primitive[Expr](name) {
    def extract = { case Lisp(Seq(Op, arg)) => arg }
  }

  sealed abstract class Primitive2(name: String) extends Primitive[(Expr,Expr)](name) {
    def extract = { case Lisp(Seq(Op, a, b)) => (a, b) }
  }

  sealed abstract class PrimitiveN(name: String) extends Primitive[Seq[Expr]](name) {
    def extract = { case Lisp(Op +: args) => args }
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

  // (lambda (p1 ... pn) e)
  object Lambda extends Primitive[(Seq[Expr],Expr)]("lambda") {
    def extract = { case Lisp(Seq(Lambda.Op, Lisp(parms), expr)) => (parms, expr) }
  }

  // (quote (lambda (p1 ... pn) e))
  object QuotedLambda extends Primitive[(Seq[Expr],Expr)]("quote") {
    def extract = { case Lisp(Seq(Op, Lambda(parms, expr))) => (parms, expr) }
  }

  object Function extends Extractable[(Seq[Expr],Expr,Seq[Expr])] {
    def extract = {
      case Lisp(Lambda(parms, expr) +: args) =>
        (parms, expr, args)
    }
  }

  object QuotedFunction extends Extractable[(Seq[Expr],Atom,Seq[Expr],Seq[Expr])] {
    def extract = {
      case Lisp(Lambda(parms, Lisp((op : Atom) +: fargs)) +: args)
        if !PrimitiveOps.contains(op) && args.nonEmpty && QuotedLambda.is(args.head) =>
          (parms, op, fargs, args)
    }
  }
  
  def eval(e: Expr): Expr = e match {

    // quoted function calls first - unquote the lambda
    case QuotedFunction(parms, op, fargs, args) =>
      val Lisp(Seq(Quote.Op, lambda)) = args.head
      eval(Lisp(lambda +: fargs))

    // function calls second - replace the expression args
    case Function(parms, expr, args) =>
      val replaced = replace(expr, parms.zip(args.map({
          case q @ Lisp(Seq(Quote.Op, _)) => q
          case e : Expr                   => eval(e)
        })).toMap)
      eval(replaced)

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
      case l: Lisp              => l.subs.head
      case _ => sys.error("not a list")
    }
    case Cdr(arg) => eval(arg) match {
      case l: Lisp if l.subs.size <= 1 => sys.error("cdr on empty or singleton list")
      case l: Lisp                     => Lisp(l.subs.tail)
      case _ => sys.error("not a list")
    }
    case Cons(a, b) => (eval(a), eval(b)) match {
      case (e, Lisp(es)) => Lisp(e +: es)
      case _ => sys.error("not a list")
    }
    case Cond(args) => {
      val Lisp(Seq(_, expr)) =
        args.find(l => l match {
          case Lisp(Seq(p, e)) => eval(p) == t
          case _ => sys.error(s"no list of sequence $l")
        }).getOrElse(sys.error("undefined"))

      eval(expr)
    }
    case _ => sys.error(s"invalid expression: $e")
  }

  def eval(s: String): Expr =
    eval(Parser.parseLisp(s))

  def replace(expr: Expr, parms: Map[Expr, Expr]): Expr = {
    Lisp(expr.subs.map({ // TODO clearly Lisp needs a `map`.
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
