import sbt._
import sbtassembly.Plugin._
import AssemblyKeys._

object SpectroBuild extends Build {

  // some crap to remove unused oscar features because i couldn't compile it
  val rootDiscards = Set("com", "ie", "JSci", "documentation","examples", "lpsolve")
  val orgDiscards =Set("apache", "gnu", "jdesktop", "jfree","w3c")
  val oscardDiscards = Set("algebra", "cbls", "demo", "examples", "linprog")

  lazy val root = Project(id = "freespectro",
                          base = file("."),
                          settings = Project.defaultSettings ++ assemblySettings ++ Seq(distTask)) settings(
  mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
    case PathList("scala" , _*) => MergeStrategy.first
    case PathList("library.properties", _*) => MergeStrategy.first
    case PathList("org", x , _*) if orgDiscards.contains(x) => MergeStrategy.discard
    case PathList("oscar", x , _*) if oscardDiscards.contains(x)  => MergeStrategy.discard
    case PathList(x, _*) if rootDiscards.contains(x) => MergeStrategy.discard
    case x => old(x)
  }
  })

  val dist = TaskKey[Unit]("dist", "")

  val distTask = dist := {
    println("TODO")
  }
}
