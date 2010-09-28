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

  "parse element name" in {
    val p = SelectorParser("div#bar")

    p(<div>text</div>) must beFalse
    p(<div id="bar">text</div>) must beTrue
  }

  "parse class" in {
    val p = SelectorParser(".bar")

    p(<div class="baz" >text</div>) must beFalse
    p(<div id="foo" class="baz bar">text</div>) must beTrue
  }

  "parse multiple classes" in {
    val p = SelectorParser("div.foo.fraw")

    p(<div class="foo"></div>) must beFalse
    p(<div class="fraw foo"></div>) must beTrue
  }

  "parse namespace prefix" in {
    val p = SelectorParser("fork|div#foo.bar")

    p(<div id="foo" class="baz bar">text</div>) must beFalse
    p(<fork:div id="foo" class="baz bar">text</fork:div>) must beTrue
  }
}
