package org.koeninger

import org.koeninger.SelectorParser._
import scala.xml._
import org.specs._

class SelectorParserSpec extends Specification {
  "parse id" in { 
    val f: Node => Boolean = SelectorParser("#foo")

    f(<div>text</div>) must beFalse
    f(<div id="foo">text</div>) must beTrue
  }

  "parse class" in {
    val g: Node => Boolean = SelectorParser(".bar")

    g(<div class="baz" >text</div>) must beFalse
    g(<div id="foo" class="baz bar">text</div>) must beTrue
  }

  "parse namespace prefix" in {
    val h: Node => Boolean = SelectorParser("fork|div#foo.bar")

    h(<div id="foo" class="baz bar">text</div>) must beFalse
    h(<fork:div id="foo" class="baz bar">text</fork:div>) must beTrue
  }
}
