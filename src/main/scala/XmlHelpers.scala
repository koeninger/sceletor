package org.koeninger

import scala.xml._
import scala.xml.transform._

object XmlHelpers {
  def buildPredicate(attr: String, value: String): Node => Boolean = n =>
    n.attribute(attr).map(_.text.split("""\s""").contains(value)).getOrElse(false)

  def buildPredicate(selector: String): Node => Boolean = selector.headOption match {
    case Some('.') => buildPredicate("class", selector.tail)
    case Some('#') => buildPredicate("id", selector.tail)
    case _ => throw new Exception("""invalid selector "%s", must start wih '.' or '#'""".format(selector))
  }

  class RichNodeSeq(xs: NodeSeq) {

    private def finder(predicate: Node => Boolean) = 
      xs flatMap (_.descendant_or_self) filter predicate

    def \\@(attr: String, value: String): NodeSeq =
      finder(buildPredicate(attr, value))

    def \\@(selector: String): NodeSeq =
      finder(buildPredicate(selector))

    def rewrite(p: Node => Boolean)(f: Node => Seq[Node]): NodeSeq = {
      object rr extends RewriteRule {
        override def transform(n: Node): Seq[Node] = if (p(n)) f(n) else n
      }
      object rt extends RuleTransformer(rr)
      rt.transform(this.xs)
    }

    def rewrite(predicates: List[Node => Boolean])(f: Node => Seq[Node]): NodeSeq = predicates match {
      case Nil => NodeSeq.Empty
      case p::Nil => rewrite(p)(f)
      case p::ps => rewrite(p)(y => y.rewrite(ps)(f))
    }
    
    def rewrite(selector: String)(f: Node => Seq[Node]): NodeSeq =
      rewrite(selector.split("""\s"""))(f)

    def rewrite(selectors: Array[String])(f: Node => Seq[Node]): NodeSeq =
      rewrite(selectors.map(s => buildPredicate(s)).toList)(f)

    def rewrite(attr: String, value: String)(f: Node => Seq[Node]): NodeSeq =
      rewrite(buildPredicate(attr, value))(f)

    def rewrite(selector: String, x: NodeSeq): NodeSeq =
      rewrite(buildPredicate(selector))({ ignore: Node => x })


  }
  implicit def richNodeSeq(xs: NodeSeq): RichNodeSeq = new RichNodeSeq(xs)
}
