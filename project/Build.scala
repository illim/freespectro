import sbt._
import sbtassembly.Plugin._
import AssemblyKeys._
import java.io.File
import sbt.Keys._

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
    case PathList("Images", _*)  => MergeStrategy.discard
    case PathList(x, _*) if rootDiscards.contains(x) => MergeStrategy.discard
    case PathList(x @ _*) if x.size == 1 => MergeStrategy.discard
    case x => old(x)
  }
  })

  val dist = TaskKey[Unit]("dist", "")
  val distTask = dist := {
    val dest = new File("d:/tmp/freespectro")
    val res = new File(dest, "resources")
    IO.delete(res)
    IO.copyDirectory((resourceDirectory in Compile).value, res)
    val jarname = (jarName in assembly).value
    IO.copyFile(new File((crossTarget in Compile).value, jarname), new File(dest, jarname))
  }
}
