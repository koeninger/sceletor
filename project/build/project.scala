import sbt._

class XmlHelpersProject(info: ProjectInfo) extends DefaultProject(info) {
  override def libraryDependencies = Set(
    "org.scala-tools.testing" %% "specs" % "1.6.5" % "test"
  ) ++ super.libraryDependencies
}
