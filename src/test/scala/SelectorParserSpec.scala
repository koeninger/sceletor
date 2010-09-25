package org.koeninger

import org.koeninger.SelectorParser._

import scala.xml._

object SelectorParserSpec {
  def apply() = {
    val f: Node => Boolean = SelectorParser("#foo")

    assert(f(<div>text</div>) == false)
    assert(f(<div id="foo">text</div>) == true)

    val g: Node => Boolean = SelectorParser(".bar")

    assert(g(<div class="baz" >text</div>) == false)
    assert(g(<div id="foo" class="baz bar">text</div>) == true)

    val h: Node => Boolean = SelectorParser("fork|div#foo.bar")

    assert(h(<div id="foo" class="baz bar">text</div>) == false)
    assert(h(<fork:div id="foo" class="baz bar">text</fork:div>) == true)

    true
  }
}
