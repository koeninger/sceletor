package org.koeninger

import scala.xml._
import XmlHelpers._
import org.specs._

class XmlHelpersSpec extends Specification {
  val x = 
    <html>
      <head><title class="too">a title</title></head>
      <body class="boo hoo">
        <div class="doo">
          a div <div class="doo noo">with a nested div</div> in it
        </div>
        <div class="doo">
          different div <weird:div id="zoo" class="doo too">different nested div</weird:div> in it
        </div>
      </body>
    </html>

  "find stuff" in {
    val target = <weird:div id="zoo" class="doo too">different nested div</weird:div>
    x.find("#zoo").head must_== target
    x.find(".doo weird|div#zoo.too").head must_== target
    x.find("div.doo.too").head must_== target
  }

  "edit stuff" in { 
    val spans = List("mopey", "dopey", "gropey")

    val one = x.edit(".doo"){ y =>
      y.edit("weird|div#zoo.too"){ z =>
        spans.map{ s => <span>{ Text(s) }</span> }}}

    val two = x.edit(".doo weird|div#zoo.too"){ y =>
        spans.map{ s => <span>{ Text(s) }</span> }}

    one must_== two
    one must_!= x
  }
}
