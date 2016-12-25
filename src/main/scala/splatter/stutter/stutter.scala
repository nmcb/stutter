package splatter.stutter

sealed trait Expr {
  def replace(parms: Map[Expr, Expr]): Expr
}
case class Atom(value: String) extends Expr {
  override def replace(parms: Map[Expr, Expr]): Expr = this
  override def toString: String = value
}
case class Lisp(expressions: Seq[Expr]) extends Expr {
  def isEmpty: Boolean = expressions.isEmpty
  override def toString: String = expressions.mkString("(", " ", ")")
  def replace(parms: Map[Expr, Expr]): Lisp = {
    Lisp(expressions.map({
      case a: Atom if parms.keySet.contains(a) => parms(a)
      case a: Atom => a
      case l: Lisp => l.replace(parms)
    }))
  }
}

object Stutter {
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

  def eval(e: Expr): Expr = e match {
    // self defined first
    case FunctionCall(parms, exp, args) => FunctionCall.yields(parms, exp, args)
    // primitives last
    case QuoteExpr(x) => QuoteExpr.yields(x)
    case AtomExpr(x) => AtomExpr.yields(x)
    case EqExpr((x, y)) => EqExpr.yields(x, y)
    case CarExpr(x) => CarExpr.yields(x)
    case CdrExpr(x) => CdrExpr.yields(x)
    case ConsExpr((x, y)) => ConsExpr.yields(x, y)
    case CondExpr(args) => CondExpr.yields(args)
    case e: Expr => sys.error("invalid expression: " + e)
  }

  abstract class ArgExtractor1(val operator: Atom) {
    def apply(arg: Expr): Lisp = Lisp(Seq(operator, arg))
    def unapply(e: Expr): Option[Expr] = e match {
      case Lisp(Seq(`operator`, arg)) => Some(arg)
      case _ => None
    }
  }

  abstract class ArgExtractor2(val operator: Atom) {
    def apply(args: (Expr, Expr)): Lisp = Lisp(Seq(operator, args._1, args._2))
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case Lisp(Seq(`operator`, arg1, arg2)) => Some((arg1, arg2))
      case _ => None
    }
  }

  object QuoteExpr extends ArgExtractor1(QuoteOp) {
    def yields(arg: Expr): Expr = arg
  }

  object AtomExpr extends ArgExtractor1(AtomOp) {
    def yields(arg: Expr): Expr = eval(arg) match {
      case a: Atom => t
      case l: Lisp if l.isEmpty => t
      case _ => f
    }
  }

  object EqExpr extends ArgExtractor2(EqOp) {
    def yields(x: Expr, y: Expr): Expr = (eval(x), eval(y)) match {
      case (a1: Atom, a2: Atom) if a1 == a2 => t
      case (l1: Lisp, l2: Lisp) if l1.isEmpty && l2.isEmpty => t
      case _ => f
    }
  }

  object CarExpr extends ArgExtractor1(CarOp) {
    def yields(x: Expr): Expr = eval(x) match {
      case l: Lisp if l.isEmpty => sys.error("car on empty list")
      case l: Lisp => l.expressions.head
      case _ => sys.error("not a list")
    }
  }

  object CdrExpr extends ArgExtractor1(CdrOp) {
    def yields(x: Expr): Expr = eval(x) match {
      case l: Lisp if l.expressions.size <= 1 => sys.error("cdr on empty or singleton list")
      case l: Lisp => Lisp(l.expressions.tail)
      case _ => sys.error("not a list")
    }
  }

  object ConsExpr extends ArgExtractor2(ConsOp) {
    def yields(x: Expr, y: Expr): Expr = (eval(x), eval(y)) match {
      case (e: Expr, l: Lisp) => Lisp(e +: l.expressions)
      case _ => sys.error("not a list")
    }
  }

  object CondExpr {
    def unapply(e: Expr): Option[Seq[Expr]] = e match {
      case Lisp(Seq(`CondOp`, args@_*)) => Some(args)
      case _ => None
    }
    def yields(args: Seq[Expr]): Expr = {
      args.find({
        case CondArgExpr(p, _) => eval(p) == t
        case arg => sys.error("not a cond, i.e. a (p e) expression, argument: " + arg)
      }).map({
        case CondArgExpr(_, e) => eval(e)
      }).getOrElse(f)
    }
    object CondArgExpr {
      def unapply(e: Expr): Option[(Expr, Expr)] = e match {
        case Lisp(Seq(p: Expr, e: Expr)) => Some((p, e))
        case _ => None
      }
    }
  }

  object FunctionCall {

    /* A function is expressed as ```(lambda (p1...pn) e)```, where
     * ```p1...pn``` are atoms (called parameters) and e is an expr.
     */
    object LambdaExpr {
      def unapply(expr: Expr): Option[(Seq[Expr], Expr)] = expr match {
        case Lisp(Seq(`LambdaOp`, Lisp(parms), e)) => Some(parms, e)
        case _ => None
      }
    }

    def isQuotedLambda(expr: Expr): Boolean = expr match {
      case QuoteExpr(LambdaExpr((_,_))) => true
      case _ => false
    }

    /* An expression whose first element is such an expression
     * ```((lambda (p1...pn) e) a1...an)``` is called a function
     * call and its value is computed (yielded) as follows below.
     */
    def unapply(expr: Expr): Option[(Seq[Expr], Expr, Seq[Expr])] = expr match {
      case Lisp(Seq(LambdaExpr(parms, e), args@_*)) => Some(parms, e, args)
      case _ => None
    }

    /* Each expression ai is evaluated. Then e is evaluated. During the
     * evaluation of e, the value of any occurrence of one of the pi is
     * the value of the corresponding ai in the most recent function call.
     * This is the parameters as arguments case below.
     *
     * If an expression has as its first element an atom op that is not one
     * of the primitive operators ```(op a1...an)``` and the value of op is
     * a function ```(lambda (p1...pn) e)``` then the value of the expression
     * is the value of ```((lambda (p1...pn) e) a1...an)```, In other words,
     * the parameters as operators in expressions case below.
     */
    def yields(parms: Seq[Expr], e: Expr, args: Seq[Expr]): Expr = e match {

      // parameters as operators.
      case Lisp(Seq(op : Atom, fargs @ _*))
        if !PrimitiveOps.contains(op) && args.nonEmpty && isQuotedLambda(args.head) =>
          // unquote the lambda so that it can be evaluated
          val QuoteExpr(lambda) = args.head
          eval(Lisp(lambda +: fargs))

      // parameters as arguments.
      case _ => eval(e.replace(parms.zip(args.map({
        // omit quoted expressions during replacement evaluation
        case QuoteExpr(quote) => QuoteExpr(quote)
        case e: Expr => eval(e)
      })).toMap))
    }
  }

  object Parser {
    import fastparse.all._
    import fastparse.core.Parsed.{Failure, Success}

    val WhiteSpace: Seq[Char] = Seq('\r', '\n', '\t', '\f', '\b', ' ')
    val Characters: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z')

    val noop: P[Unit] = P(CharIn(WhiteSpace).rep.?)
    val char: P[Unit] = P(CharIn(Characters))

    val atom: P[Atom] = P(char.rep(1).!.map(Atom))
    val list: P[Lisp] = P("(" ~ expr.rep.map(Lisp) ~ noop ~ ")")
    val quot: P[Lisp] = P(("'" | "’") ~ expr.map(e => QuoteExpr(e)))
    val expr: P[Expr] = P(noop ~ (atom | list | quot) ~ noop)

    def parse(s: String): Expr = expr.parse(s) match {
      case Success(e, _) => e
      case f: Failure[_, _] => sys.error(f.msg)
    }
  }
}
