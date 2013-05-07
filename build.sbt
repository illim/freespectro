name := "freespectro"

version := "1.0"

scalaVersion := "2.10.1"

resolvers ++= Seq(
 "Sonatype Repository" at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq(
 "org.lwjgl.lwjgl" % "lwjgl" % "2.8.2",
 "org.lwjgl.lwjgl" % "lwjgl_util" % "2.8.2",
 "org.lwjgl.lwjgl" % "lwjgl-platform" % "2.8.2" classifier "natives-windows",
 "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
 "org.scalaz"      % "scalaz-core_2.10" % "7.0.0-M7")

fork in run := true

javaOptions in run ++= Seq("-Djava.library.path="+Path.userHome.absolutePath+"/.ivy2/cache/org.lwjgl.lwjgl/lwjgl-platform/jars/", "-Dorg.lwjgl.util.Debug=true")

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.1")

scalacOptions ++= Seq("-P:continuations:enable")

