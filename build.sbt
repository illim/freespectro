import AssemblyKeys._
name := "freespectro"

version := "1.0"

scalaVersion := "2.10.0"

resolvers ++= Seq(
 "Sonatype Repository" at "http://oss.sonatype.org/content/repositories/releases")

val lvers = "2.9.1"

libraryDependencies ++= Seq(
 "org.lwjgl.lwjgl" % "lwjgl" % lvers,
 "org.lwjgl.lwjgl" % "lwjgl_util" % lvers,
 "org.lwjgl.lwjgl" % "lwjgl-platform" % lvers classifier "natives-windows",
 "org.scalatest"   % "scalatest_2.10" % "1.9.1" % "test",
 "org.scalaz"      % "scalaz-core_2.10" % "7.0.5")

fork in run := true

javaOptions in run ++= Seq("-Djava.library.path="+Path.userHome.absolutePath+"/.ivy2/cache/org.lwjgl.lwjgl/lwjgl-platform/jars/", "-Dorg.lwjgl.util.Debug=true")

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0")

scalacOptions ++= Seq("-P:continuations:enable")

test in assembly := {}