package info
package object sceletor {
  import info.sceletor.RichNodeSeq
  import scala.xml._

  implicit def richNodeSeq(xs: NodeSeq): RichNodeSeq = new RichNodeSeq(xs)

  def find(selector: String) =
    (xs: NodeSeq) => (new RichNodeSeq(xs)).find(selector)

  def edit(selector: String, replace: NodeSeq) =
    (xs: NodeSeq) => (new RichNodeSeq(xs)).edit(selector, replace)

  def chain(fns: NodeSeq => NodeSeq*) =
    Function.chain(fns)

  implicit def pairToFn(p:(String, NodeSeq)): NodeSeq => NodeSeq =
    edit(p._1, p._2)

  implicit def pairToFn2(p:(String, NodeSeq => NodeSeq)): NodeSeq => NodeSeq =
    (xs: NodeSeq) => (new RichNodeSeq(xs)).edit(p._1)(p._2)

  def kid(contents: NodeSeq): NodeSeq => NodeSeq = 
    (xs: NodeSeq) => xs.flatMap{ x => x match{
      case e @ Elem(p, l, a, s, _) => Elem(p, l, a, s, contents: _*)
      case a => a
    }}
  def kid(contents: String): NodeSeq => NodeSeq = kid(Text(contents))

}
