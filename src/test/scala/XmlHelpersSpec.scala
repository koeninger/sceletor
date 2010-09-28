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

  "rewrite stuff" in { 
    val spans = List("mopey", "dopey", "gropey")

    val one = x.rewrite(".doo"){ y =>
      y.rewrite("weird|div#zoo.too"){ z =>
        spans.map{ s => <span>{ Text(s) }</span> }}}

    val two = x.rewrite(".doo weird|div#zoo.too"){ y =>
        spans.map{ s => <span>{ Text(s) }</span> }}

    one mustEqual two
  }
}
