package org.koeninger

import org.koeninger.SelectorParser._

import scala.xml._

object SelectorParserSpec {
  def apply() = {
    val f: Node => Boolean = SelectorParser(hash, "#foo").get.asInstanceOf[Node => Boolean]

    assert(f(<div>text</div>) == false)
    assert(f(<div id="foo">text</div>) == true)

    val g: Node => Boolean = SelectorParser(klass, ".bar").get.asInstanceOf[Node => Boolean]

    assert(g(<div class="baz" >text</div>) == false)
    assert(g(<div id="foo" class="baz bar">text</div>) == true)

    val h: Node => Boolean = SelectorParser(simpleSelectorSequence, "fork|div#foo.bar").get.asInstanceOf[Node => Boolean]

    assert(h(<div id="foo" class="baz bar">text</div>) == false)
    assert(h(<fork:div id="foo" class="baz bar">text</fork:div>) == true)

    true
  }
}
