package org.koeninger

import scala.xml._
import scala.xml.transform._

object XmlHelpers {
  class RichNodeSeq(xs: NodeSeq) {
    private def buildPredicate(attr: String, value: String): Node => Boolean = n =>
      n.attribute(attr).map(_.text.split("""\s""").contains(value)).getOrElse(false)

    private def buildPredicate(selector: String): Node => Boolean = selector.headOption match {
      case Some('.') => buildPredicate("class", selector.tail)
      case Some('#') => buildPredicate("id", selector.tail)
      case _ => throw new Exception("""invalid selector "%s", must start wih '.' or '#'""".format(selector))
    }

    private def finder(predicate: Node => Boolean) = 
      xs flatMap (_.descendant_or_self) filter predicate

    def \\@(attr: String, value: String): NodeSeq =
      finder(buildPredicate(attr, value))

    def \\@(selector: String): NodeSeq =
      finder(buildPredicate(selector))

    def rewrite(p: Node => Boolean, f: Node => Seq[Node]): NodeSeq = {
      object rr extends RewriteRule {
        override def transform(n: Node): Seq[Node] = if (p(n)) f(n) else n
      }
      object rt extends RuleTransformer(rr)
      this.xs.map(rt(_))
    }
    
    def rewrite(selector: String, f: Node => Seq[Node]): NodeSeq =
      rewrite(buildPredicate(selector), f)

    def rewrite(attr: String, value: String, f: Node => Seq[Node]): NodeSeq =
      rewrite(buildPredicate(attr, value), f)

    def rewrite(selector: String, x: NodeSeq): NodeSeq =
      rewrite(buildPredicate(selector), { ignore: Node => x })


  }
  implicit def richNodeSeq(xs: NodeSeq) = new RichNodeSeq(xs)
}
