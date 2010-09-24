package org.koeninger

import scala.xml._
import XmlHelpers._

object XmlHelpersSpec {
  val x = 
    <html>
      <head><title class="too">a title</title></head>
      <body class="boo hoo">
        <div class="doo">
          a div <div class="doo noo">with a nested div</div> in it
        </div>
        <div class="doo">
          different div <div class="doo too">different nested div</div> in it
        </div>
      </body>
    </html>

  val spans = List("mopey", "dopey", "gropey")

  def apply() = {
    x.rewrite(".doo"){ y =>
      y.rewrite(".too"){ z =>
        spans.map{ s => <span class={ s }>{ z.child }</span> }}}
  }
}
