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
  def apply() = {
    x rewrite(".doo", { y: Node =>
      y rewrite(".too", { z: Node =>
        <span class="replaced">{ z.child }</span> })})
  }
}
