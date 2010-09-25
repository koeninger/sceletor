package org.koeninger

import scala.util.parsing.combinator._

/** http://www.w3.org/TR/css3-selectors/#w3cselgrammar */
object SelectorParser extends RegexParsers {

  // tokenizer, ignoring nonascii and escape for now
  val ident: Parser[Any] = """-?""".r ~ nmstart ~ rep(nmchar)
  val nmstart: Parser[Any] = """[_a-zA-Z]""".r
  val nmchar: Parser[Any] = """[_a-zA-Z0-9-]""".r
  val name: Parser[Any] = rep1(nmchar)
  val hash: Parser[Any] = "#" ~ name

  // productions, just doing simple selector for now
  // XXX whats the equivalent of regex '?'
  val namespacePrefix: Parser[Any] = (
    ((ident | "*") ~ "|")
    | "|"
  )
  val elementName: Parser[Any] = ident
  val typeSelector: Parser[Any] = namespacePrefix ~ elementName
  val universal: Parser[Any] = (
    namespacePrefix ~ "*"
    | "*"
  )
  val klass: Parser[Any] = "." ~ ident
  val attrib: Parser[Any] = failure("only type selector, id and class are implemented")
  val pseudo: Parser[Any] = failure("only type selector, id and class are implemented")
  val negation: Parser[Any] = failure("only type selector, id and class are implemented")

  val simpleSelectorSequence = (
    (typeSelector | universal) ~ rep(hash | klass | attrib | pseudo | negation)
    | rep1(hash | klass | attrib | pseudo | negation)
  )

  def apply(p: Parser[Any], s: String) = parseAll(p, s)
}
