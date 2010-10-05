package org.koeninger

import scala.xml._
import scala.xml.transform._

object XmlHelpers {
  class RichNodeSeq(xs: NodeSeq) {

    private def finder(predicate: Node => Boolean) = 
      xs flatMap (_.descendant_or_self) filter predicate

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

    def edit(selector: String, x: NodeSeq): NodeSeq =
      edit(selector)({ ignore: Node => x })

  }
  implicit def richNodeSeq(xs: NodeSeq): RichNodeSeq = new RichNodeSeq(xs)
}
