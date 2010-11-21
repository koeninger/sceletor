package info.sceletor

// find is defined by specs...
import info.sceletor.{ find => fine }
import org.specs._
import scala.xml._

class RichNodeSeqSpec extends Specification {
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

  
  "implicitly added nodeseq methods" should {

    "find stuff" in {
      val target = <weird:div id="zoo" class="doo too">different nested div</weird:div>
      x.find("#zoo").head must_== target
      x.find(".doo weird|div#zoo.too").head must_== target
      x.find("div.doo.too").head must_== target
    }

    "edit stuff with literals" in {
      x.edit("title.too", <title>A new title</title>).toString mustMatch "<title>A new title</title>"
    }

    "edit stuff with functions" in { 
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

  "combinators" should {

    "find stuff" in {
      val target = <weird:div id="zoo" class="doo too">different nested div</weird:div>

      fine("#zoo")(x).head must_== target
      fine(".doo weird|div#zoo.too")(x).head must_== target
      fine("div.doo.too")(x).head must_== target
    }

    "edit stuff with literals" in {
      edit("title.too", <title>A new title</title>)(x).toString mustMatch "<title>A new title</title>"
    }

    "chain together edits and finds" in {
      val target: NodeSeq =        
        <div class="doo">
          a div <p>noo</p> in it
        </div>
        <div class="doo">
          different div <p>too</p> in it
        </div>;

      chain(
        fine("div[class=doo]"),
        edit(".noo", <p>noo</p>),
        edit(".too", <p>too</p>)
      )(x) must_== target

      chain(
        fine("div[class=doo]"),
        ".noo" -> <p>noo</p>,
        ".too" -> <p>too</p>
      )(x) must_== target

      def pWrap(s: NodeSeq): NodeSeq =
        <p>{ s.head.attributes("class").head.toString.split(" ")(1) }</p>

      chain(
        fine("div[class=doo]"),
        ".noo" -> pWrap _,
        ".too" -> pWrap _
      )(x) must_== target

    }
  }
}
