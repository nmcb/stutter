package splatter
package stutter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ExpressionAxioms extends AnyFunSpec:
  
  import Parser.*
  import Expr.*

  describe("Expression axioms"):
    it("quote expressions should yield the quoted expression"):
      "(quote a)".parse.eval.toString       should be ("a")
      "'a".parse.eval.toString              should be ("a")
      "(quote (a b c))".parse.eval.toString should be ("(a b c)")
      
    it("atom expressions should yield the atom `t` if the argument yields an atom or the empty list"):
      "(atom 'a)".parse.eval.toString        should be ("t")
      "(atom '())".parse.eval.toString       should be ("t")
      "(atom (atom 'a))".parse.eval.toString should be ("t")
      
    it("atom expression should yield the empty list if the argument does not yield an atom or the empty list"):
      "(atom '(atom a))".parse.eval.toString should be ("()")
      "(atom '(a b c))".parse.eval.toString  should be ("()")
    
    it("eq expression should yield the atom `t` if both arguments yield the same atom or both the empty list"):
      "(eq 'a 'a)".parse.eval.toString   should be ("t")
      "(eq '() '())".parse.eval.toString should be ("t")
    
    it("eq expression should yield the empty list if both arguments do not yield the same atom or the empty list"):
      "(eq 'a 'b)".parse.eval.toString should be ("()")
    
    it("car expression should yield the first element of its argument list"):
      "(car '(a b c))".parse.eval.toString should be ("a")
    
    it("cdr expression should yield everything after the first element of its argument list"):
      "(cdr '(a b c))".parse.eval.toString should be ("(b c)")
    
    it("cons expression should return a list containing the value of its first argument followed by the elements of the value of its second argument"):
      "(cons 'a '(b c))".parse.eval.toString                  should be ("(a b c)")
      "(cons 'a (cons 'b (cons 'c '())))".parse.eval.toString should be ("(a b c)")
      "(car (cons 'a '(b c)))".parse.eval.toString            should be ("a")
      "(cdr (cons 'a '(b c)))".parse.eval.toString            should be ("(b c)")
    
    it("cond expression (cond (p1 e1)...(pn en)) is evaluated as follows; the p expressions are evaluated in order until one returns t; when one is found, the value of the corresponding e expression is returned as the value of the whole cond expression."):
      "(cond ((eq 'a 'b) 'first) ((atom 'a) 'second))".parse.eval.toString should be ("second")
    