Bare bones library to find and edit xml nodes using CSS3 selectors.

```

scala> import info.sceletor._
import info.sceletor._

scala> val page = <html><head></head><body><h1 class="hello">placeholder</h1></body></html>
page: scala.xml.Elem = <html><head></head><body><h1 class="hello">placeholder</h1></body></html>

scala> page.find(".hello")                                                                 
res2: scala.xml.NodeSeq = NodeSeq(<h1 class="hello">placeholder</h1>)

scala> page.edit(".hello", <p>Hello World</p>)                                             
res3: scala.xml.NodeSeq = NodeSeq(<html><head></head><body><p>Hello World</p></body></html>)
```
