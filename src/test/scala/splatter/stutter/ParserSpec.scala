package splatter
package stutter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ParserSpec extends AnyFunSpec:
  
  import Parser._
  
  describe("Parser"):
    it("should parse atoms"):
      "foo".parse should be(
        Atom("foo"))
    
    it("should parse lists"):
      "()".parse should be(
        Lisp(Nil))
      "(foo)".parse should be(
        Lisp(Seq(Atom("foo"))))
      "(foo bar)".parse should be(
        Lisp(Seq(Atom("foo"), Atom("bar"))))
      "(a b (c) d)".parse should be(
        Lisp(Seq(Atom("a"), Atom("b"), Lisp(Seq(Atom("c"))), Atom("d"))))
    
    it("should parse normal single ' quotes on atoms and lists"):
      "'a".parse should be(
        Lisp(Seq(Atom("quote"), Atom("a"))))
      "'(a b c)".parse should be(
        Lisp(Seq(Atom("quote"), Lisp(Seq(Atom("a"), Atom("b"), Atom("c"))))))
    
    it("should parse ugly, copy-paste, english, burn your fingers, sharp ’ quotes"):
      "’a".parse should be(
        Lisp(Seq(Atom("quote"), Atom("a"))))
    
    it("should parse complex structures"):
      "((lambda (x) (cons x '(b))) 'a)".parse should be(
        Lisp(Seq(
          Lisp(Seq(
            Atom("lambda"),
            Lisp(Seq(Atom("x"))),
            Lisp(Seq(
              Atom("cons"),
              Atom("x"),
              Lisp(Seq(Atom("quote"),
                Lisp(Seq(
                  Atom("b")
                ))
              ))
            ))
          )),
          Lisp(Seq(Atom("quote"), Atom("a")))
        ))
      )
    
    it("should handle complex whitespace"):
      "(  a  )".parse should be ("(a)".parse)
      """'(
          |  a
          |     b
          |        c
          |)""".stripMargin.parse should be("'(a b c)".parse)
    
    /* Sanity Checks */

    it("should parse lists recursively"):
      "(())".parse should be(Lisp(Seq(Lisp(Nil))))
