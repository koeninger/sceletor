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
  val includes: Parser[String] = "~="
  val dashmatch: Parser[String] = "|="
  val prefixmatch: Parser[String] = "^="
  val suffixmatch: Parser[String] = "$="
  val substringmatch: Parser[String] = "*="

  val alwaysTrue = (n: Node) => true

  // productions, just doing simple selector for now

  // behavior of namespace prefix is different for nodes vs attributes
  val namespacePrefixNode: Parser[Node => Boolean] =
    opt(ident | "*") <~ "|" ^^ {
      case Some("*") => alwaysTrue
      case Some(s) => (n: Node) => n.prefix == s
      case None => (n: Node) => n.prefix == null
    }

  val namespacePrefixAttrib: Parser[Node => MetaData] =
    opt(ident | "*") <~ "|" ^^ {
      case Some("*") => _.attributes
      case Some(s) => _.attributes filter {
        case PrefixedAttribute(p, _, _, _) if p == s => true
        case _ => false
      }
      case None => _.attributes filter {
        case UnprefixedAttribute(_, _, _) => true
        case _ => false
      }
    }

  val elementName: Parser[Node => Boolean] = ident ^^ { s => (n: Node) => n.label == s }
  val typeSelector: Parser[Node => Boolean] = (
    namespacePrefixNode ~ elementName ^^ { case x~y => (n: Node) => x(n) && y(n) }
    | elementName
  )
  val universal: Parser[Node => Boolean] = (
    namespacePrefixNode <~ "*"
    | "*" ^^ { case _ => alwaysTrue }
  )
  val klass: Parser[Node => Boolean] = "." ~> ident ^^ { s => buildPredicate("class", s) }
  val attrib: Parser[Node => Boolean] =
    "[" ~> opt(namespacePrefixAttrib) ~ ident <~ "]" ^^ {
      case Some(p)~s => (n: Node) => p(n).filter(_.key == s) != Null
      case None~s => (n: Node) => n.attribute(s).isDefined 
    }
  val pseudo: Parser[Any] = failure("pseudo isnt implemented")
  val negation: Parser[Any] = failure("negation isnt implemented")

  def andAll(xs: List[Node => Boolean]): Node => Boolean = (n: Node) => xs.forall{ _(n) }

  val simpleSelectorSequence: Parser[Node => Boolean] = (
    (typeSelector | universal) ~ rep(hash | klass | attrib) ^^ { case x~xs => andAll(x::xs) }
    | rep1(hash | klass | attrib) ^^ { case xs => andAll(xs) }
  )

  def apply(p: Parser[Any], s: String) = parseAll(p, s)
  def apply(s: String): Node => Boolean = parseAll(simpleSelectorSequence, s).get
}
