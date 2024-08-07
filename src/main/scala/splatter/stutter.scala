package splatter
package stutter

sealed trait Expr:
  def isAtom: Boolean
  def isLisp: Boolean
  def subs: Seq[Expr]

object Expr:
  // special values?! - relax.. these were the 1960s - legacy
  val t = Atom("t")
  val f = Lisp(Nil)

case class Atom(value: String) extends Expr:
  def isAtom: Boolean = true
  def isLisp: Boolean = false
  def subs: Seq[Expr] = Nil
  override def toString: String = value

case class Lisp(subs: Seq[Expr]) extends Expr:
  def isAtom: Boolean  = false
  def isLisp: Boolean  = true
  def isEmpty: Boolean = subs.isEmpty

  override def toString: String = subs.mkString("(", " ", ")")

sealed trait Extractable[T]:
  def extract: PartialFunction[Expr,T]

  def unapply(e: Expr): Option[T] = extract.lift(e)
  def is(e: Expr): Boolean        = unapply(e).isDefined

abstract class ExtractableOp[T](name: String) extends Extractable[T]:
  val Op: Atom = Atom(name)

case class ExtractableOp1(name: String) extends ExtractableOp[Expr](name):
  def extract = { case Lisp(Seq(Op, arg)) => arg }

case class ExtractableOp2(name: String) extends ExtractableOp[(Expr,Expr)](name):
  def extract = { case Lisp(Seq(Op, a, b)) => (a, b) }

case class ExtractableOpN(name: String) extends ExtractableOp[Seq[Expr]](name):
  def extract = { case Lisp(Op +: args) => args }

lazy val QuoteLit = ExtractableOp1("quote")
lazy val AtomLit  = ExtractableOp1("atom")
lazy val EqLit    = ExtractableOp2("eq")
lazy val CarLit   = ExtractableOp1("car")
lazy val CdrLit   = ExtractableOp1("cdr")
lazy val ConsLit  = ExtractableOp2("cons")
lazy val CondLit  = ExtractableOpN("cond")

val PrimitiveOps: Seq[Atom] =
  Seq(AtomLit.Op, QuoteLit.Op, EqLit.Op, CarLit.Op, CdrLit.Op, ConsLit.Op, CondLit.Op)

  // (lambda (p1 ... pn) e)
object Lambda extends ExtractableOp[(Seq[Expr],Expr)]("lambda"):
  def extract = { case Lisp(Seq(Op, Lisp(parms), expr)) => (parms, expr) }

// (quote (lambda (p1 ... pn) e))
object QuotedLambda extends ExtractableOp[(Seq[Expr],Expr)]("quote"):
  def extract = { case Lisp(Seq(Op, Lambda(parms, expr))) => (parms, expr) }

// ((lambda (p1 ... pn) e) a1 ... an)
object Function extends Extractable[(Seq[Expr],Expr,Seq[Expr])]:
  def extract: PartialFunction[Expr, (Seq[Expr], Expr, Seq[Expr])] = {
    case Lisp(Lambda(parms, expr) +: args) =>
      (parms, expr, args)
  }

// ((lambda (p1 ... pn) (f fa1 ... fan)) a1 ... an)
object QuotedFunction extends Extractable[(Seq[Expr],Atom,Seq[Expr],Seq[Expr])]:
  def extract: PartialFunction[Expr, (Seq[Expr], Atom, Seq[Expr], Seq[Expr])] = {
    case Lisp(Lambda(parms, Lisp((op : Atom) +: fargs)) +: args)
      if !PrimitiveOps.contains(op) &&
          args.nonEmpty &&
          QuotedLambda.is(args.head) => (parms, op, fargs, args)
  }

def eval(e: Expr): Expr =
  e match

    // quoted function calls first
    // - unquote the lambda present in the first argument
    case QuotedFunction(parms, op, fargs, args) =>
      eval(Lisp(args.head match
        case Lisp(Seq(QuoteLit.Op, lambda)) => lambda  +: fargs
        case _                              => sys.error("lambda expression not quoted")))

    // function calls second
    // - eval all args except for quoted ones
    // - replace the expression parms with the evaluated args
    case Function(parms, expr, args) =>
      val evaluated = args.map {
        case q @ QuoteLit(_) => q
        case e : Expr     => eval(e)
      }
      val replaced = replace(expr, parms.zip(evaluated).toMap)
      eval(replaced)

    // primitive operations last
    case QuoteLit(arg) => arg
    case AtomLit(arg)  => eval(arg) match
      case a: Atom              => Expr.t
      case l: Lisp if l.isEmpty => Expr.t
      case _                    => Expr.f
    case EqLit(a, b) => (eval(a), eval(b)) match
      case (a1: Atom, a2: Atom) if a1 == a2                 => Expr.t
      case (l1: Lisp, l2: Lisp) if l1.isEmpty && l2.isEmpty => Expr.t
      case _                                                => Expr.f
    case CarLit(arg) => eval(arg) match
      case l: Lisp if l.isEmpty => sys.error("car on empty list")
      case l: Lisp              => l.subs.head
      case a: Atom              => sys.error(s"not a list: $a")
    case CdrLit(arg) => eval(arg) match
      case l: Lisp if l.subs.size <= 1 => sys.error("cdr on empty or singleton list")
      case l: Lisp                     => Lisp(l.subs.tail)
      case a: Atom                     => sys.error(s"not a list: $a")
    case ConsLit(a, b) => (eval(a), eval(b)) match
      case (e, Lisp(es)) => Lisp(e +: es)
      case (_, a: Atom)  => sys.error(s"not a list: $a")
    case CondLit(args) =>
      args
        .find:
          case Lisp(Seq(p, e)) => eval(p) == Expr.t
          case e: Expr         => sys.error(s"not a conditional $e")
        .getOrElse(sys.error("undefined"))
          match
            case Lisp(Seq(_, expr)) => eval(expr)
            case e                  => sys.error(s"has no argument list $e")
    case _ =>
      sys.error(s"invalid expression: $e")

def eval(s: String): Expr =
  eval(Parser.parseLisp(s))

def replace(expr: Expr, parms: Map[Expr, Expr]): Expr =
  Lisp(expr.subs.map({ // TODO clearly Lisp needs a `map`.
    case a: Atom if parms.keySet.contains(a) => parms(a)
    case a: Atom => a
    case r: Lisp => replace(r, parms)
  }))

object Parser:
  
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
    (keyword("'") | keyword("’")) ~ expr.map(e => Lisp(Seq(QuoteLit.Op, e)))

  def expr: P[Expr] =
    for {
      _ <- spaces
      e <- (atom | list | quot)
      _ <- spaces
    } yield e

  def parseLisp(s: String): Expr =
    run(expr)(s)
