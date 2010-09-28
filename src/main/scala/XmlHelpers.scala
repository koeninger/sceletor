package org.koeninger

import scala.xml._
import scala.xml.transform._

object XmlHelpers {
  def buildPredicate(attr: String, value: String): Node => Boolean = n =>
    n.attribute(attr).map(_.text.split("""\s""").contains(value)).getOrElse(false)

  class RichNodeSeq(xs: NodeSeq) {

    private def finder(predicate: Node => Boolean) = 
      xs flatMap (_.descendant_or_self) filter predicate

    def find(attr: String, value: String): NodeSeq =
      finder(buildPredicate(attr, value))

    def find(predicates: List[Node => Boolean]): NodeSeq = predicates match {
      case Nil => NodeSeq.Empty
      case p::Nil => finder(p)
      case p::ps => finder(p).find(ps)
    }

    def find(selector: String): NodeSeq =
      find(selector.split("""\s""").toList.map{ s => SelectorParser(s) })

    def edit(p: Node => Boolean)(f: Node => Seq[Node]): NodeSeq = {
      object rr extends RewriteRule {
        override def transform(n: Node): Seq[Node] = if (p(n)) f(n) else n
      }
      object rt extends RuleTransformer(rr)
      rt.transform(this.xs)
    }

    def edit(predicates: List[Node => Boolean])(f: Node => Seq[Node]): NodeSeq = predicates match {
      case Nil => NodeSeq.Empty
      case p::Nil => edit(p)(f)
      case p::ps => edit(p)(y => y.edit(ps)(f))
    }
    
    def edit(selector: String)(f: Node => Seq[Node]): NodeSeq =
      edit(selector.split("""\s"""))(f)

    def edit(selectors: Array[String])(f: Node => Seq[Node]): NodeSeq =
      edit(selectors.map(s => SelectorParser(s)).toList)(f)

    def edit(attr: String, value: String)(f: Node => Seq[Node]): NodeSeq =
      edit(buildPredicate(attr, value))(f)

    def edit(selector: String, x: NodeSeq): NodeSeq =
      edit(SelectorParser(selector))({ ignore: Node => x })


  }
  implicit def richNodeSeq(xs: NodeSeq): RichNodeSeq = new RichNodeSeq(xs)
}
