package splatter
package parsing

case class P[+A](parse: String => Option[(A,String)]) {

  import P._

  def flatMap[B](f: A => P[B]): P[B] =
    P(s => parse(s) match {
      case Some((a,r)) => f(a).parse(r)
      case None        => None
    })

  def map[B](f: A => B): P[B] =
    P(s => parse(s) match {
      case Some((a,r)) => Some(f(a),r)
      case None        => None
    })

  def |[A1 >: A](that: => P[A1]): P[A1] =
    P(s => parse(s) match {
      case None        => that.parse(s)
      case res@Some(_) => res
    })

  def ~[B](that: P[B]): P[B] =
    for { _ <- this ; b <- that } yield b

  private def loop[A1 >: A](s: String, acc: List[A1] = List.empty): (List[A1], String) =
    parse(s) match {
      case None         => (acc.reverse, s)
      case Some((a,ss)) => loop(ss, a :: acc)
    }  

  def zeroOrMore: P[List[A]] =
    P(s => Some(loop(s)))

  def oneOrMore: P[List[A]] = {
    P(s => parse(s).flatMap { case (a,ss) => Some(loop(ss, List(a)))})
  }

  def iff[B](p: A => Boolean)(t: P[B])(f: => P[B]): P[B] =
    P(s => parse(s).flatMap { case (a,ss) => if (p(a)) t.parse(ss) else f.parse(ss)})
}

object P {

  def run[A](p: P[A])(s: String): A =
    p.parse(s) match {
      case Some((a, "")) => a
      case Some((_, rs)) => sys.error(s"unconsumed: $rs")
      case None          => sys.error(s"did not produce a value: ${s}")
    }

  def unit[A](a: A): P[A] = 
    P(s => Some(a, s))
  
  def take: P[Char] =
    P(s => if (s.nonEmpty) Some(s.head, s.tail) else None)

  def fail[A]: P[A] =
    P(_ => None)

  def satisfy(p: Char => Boolean): P[Char] =
    take.flatMap(c => if (p(c)) unit(c) else fail)

  def end: P[Boolean] =
    P(s => if (s.isEmpty) Some(true, "") else Some(false, s))

  def digit: P[Char] =
    satisfy(_.isDigit)
  
  def digits: P[Long] =
    satisfy(_.isDigit).oneOrMore.map(_.mkString("").toLong)
    
  def char(c: Char): P[Char] =
    satisfy(_ == c)

  def string(s: String): P[String] =
    if (s.isEmpty) unit("") else for { _ <- char(s.head) ; _ <- string(s.tail) } yield s

  def oneOf(s: String): P[Char] =
    satisfy(s.contains(_))

  def spaces: P[String] =
    oneOf(" \t\n\r").zeroOrMore.map(_.mkString)

  def token[A](p: P[A]): P[A] =
    for { a <- p ; _ <- spaces } yield a

  def keyword(word: String): P[String] =
    token(string(word))    
}