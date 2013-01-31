name := "2snake"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
 "org.apache.commons" % "commons-io" % "1.3.2",
 "org.lwjgl.lwjgl" % "lwjgl" % "2.8.2",
 "org.lwjgl.lwjgl" % "lwjgl_util" % "2.8.2",
 "org.lwjgl.lwjgl" % "lwjgl-platform" % "2.8.2" classifier "natives-windows")

fork in run := true

javaOptions in run ++= Seq("-Djava.library.path=C:/Users/mdoboi/.ivy2/cache/org.lwjgl.lwjgl/lwjgl-platform/jars/", "-Dorg.lwjgl.util.Debug=true")