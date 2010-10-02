package org.koeninger

import org.koeninger.SelectorParser._
import scala.xml._
import org.specs._

class SelectorParserSpec extends Specification {
  "parse id" in { 
    val p: Node => Boolean = SelectorParser("#foo")

    p(<div>text</div>) must beFalse
    p(<div id="foo">text</div>) must beTrue
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
    var p = SelectorParser("ns|e")

    p(<e />) must beFalse
    p(<ns:e />) must beTrue
    p(<any:e />) must beFalse
    p(<f />) must beFalse

    p = SelectorParser("*|e")
    p(<e />) must beTrue
    p(<any:e />) must beTrue
    p(<f />) must beFalse

    p = SelectorParser("|e")
    p(<e />) must beTrue
    p(<any:e />) must beFalse
    p(<f />) must beFalse
  }

  "parse attributes" in {
    val p = SelectorParser("[ attr ]")
    p(<div notattr="">text</div>) must beFalse
    p(<div attr="">text</div>) must beTrue
  }
}
