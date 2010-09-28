package org.koeninger

import XmlHelpers.buildPredicate
import scala.util.parsing.combinator._
import scala.xml._

/** http://www.w3.org/TR/css3-selectors/#w3cselgrammar */
object SelectorParser extends RegexParsers {

  // tokenizer, ignoring nonascii and escape for now
  val ident: Parser[String] = """-?""".r ~ nmstart ~ rep(nmchar) ^^ { case x~y~z => x + y + z.mkString }
  val nmstart: Parser[String] = """[_a-zA-Z]""".r
  val nmchar: Parser[String] = """[_a-zA-Z0-9-]""".r
  val name: Parser[String] = rep1(nmchar) ^^ { _.mkString }
  val hash: Parser[Node => Boolean] = "#" ~> name ^^ { s => buildPredicate("id", s) }

  val alwaysTrue = (n: Node) => true

  // productions, just doing simple selector for now
  // XXX whats the equivalent of regex '?'
  val namespacePrefix: Parser[Node => Boolean] = (
    ((ident | "*") <~ "|") ^^ { case "*" => alwaysTrue; case s => (n: Node) => n.prefix == s }
    | "|" ^^ { case _ => alwaysTrue }
  )
  val elementName: Parser[Node => Boolean] = ident ^^ { s => (n: Node) => n.label == s }
  val typeSelector: Parser[Node => Boolean] = (
    namespacePrefix ~ elementName ^^ { case x~y => (n: Node) => x(n) && y(n) }
    | elementName
  )
  val universal: Parser[Node => Boolean] = (
    namespacePrefix ~ "*" ^^ { case _ => alwaysTrue }
    | "*" ^^ { case _ => alwaysTrue }
  )
  val klass: Parser[Node => Boolean] = "." ~> ident ^^ { s => buildPredicate("class", s) }
  val attrib: Parser[Any] = failure("only type selector, id and class are implemented")
  val pseudo: Parser[Any] = failure("only type selector, id and class are implemented")
  val negation: Parser[Any] = failure("only type selector, id and class are implemented")

  def allAnd(xs: List[Node => Boolean]): Node => Boolean = (n: Node) => xs.forall{ _(n) }

  val simpleSelectorSequence: Parser[Node => Boolean] = (
    (typeSelector | universal) ~ rep(hash | klass) ^^ { case x~xs => allAnd(x::xs) }
    | rep1(hash | klass) ^^ { case xs => allAnd(xs) }
  )

  def apply(p: Parser[Any], s: String) = parseAll(p, s)
  def apply(s: String): Node => Boolean = parseAll(simpleSelectorSequence, s).get
}
