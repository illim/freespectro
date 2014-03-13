import sbt._
import sbtassembly.Plugin._
import AssemblyKeys._
import java.io.File
import sbt.Keys._

object SpectroBuild extends Build {


  lazy val root = Project(id = "freespectro",
                          base = file("."),
                          settings = Project.defaultSettings ++ assemblySettings ++ Seq(distTask)) settings(
  mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
    case PathList("Images", _*)  => MergeStrategy.discard
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
