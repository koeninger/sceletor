package info.sceletor

import org.specs._
import scala.xml._


class PackageSpec extends Specification {
  "sceletor" should {
    "be easy to use" in {
      val page: NodeSeq = <html><head></head><body><h1 class="hello">placeholder</h1></body></html>

      page.find(".hello") must
        ==/(<h1 class="hello">placeholder</h1>)

      page.edit(".hello", <p>Hello World</p>) must
        ==/(<html><head></head><body><p>Hello World</p></body></html>)

      chain(".hello" -> child("Hello World"))(page) must 
        ==/(<html><head></head><body><h1 class="hello">Hello World</h1></body></html>)
    }
  }
}
