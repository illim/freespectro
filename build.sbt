import AssemblyKeys._
name := "freespectro"

version := "1.0"

scalaVersion := "2.11.0"

resolvers ++= Seq(
 "Sonatype Repository" at "http://oss.sonatype.org/content/repositories/releases")

val lvers = "2.9.1"

libraryDependencies ++= Seq(
 "org.lwjgl.lwjgl" % "lwjgl" % lvers,
 "org.lwjgl.lwjgl" % "lwjgl_util" % lvers,
 "org.lwjgl.lwjgl" % "lwjgl-platform" % lvers classifier "natives-windows",
 "org.scalatest"   % "scalatest_2.11" % "2.1.3" % "test",
 "org.scalaz"      % "scalaz-core_2.11" % "7.0.6"
// "org.scalanlp"    % "breeze_2.11" % "0.8.1"
// "com.github.fommil"    % "org.scalanlp" % "1.1"
)

fork in run := true

scalacOptions += "-Ydelambdafy:method"

javaOptions in run ++= Seq("-Djava.library.path="+Path.userHome.absolutePath+"/.ivy2/cache/org.lwjgl.lwjgl/lwjgl-platform/jars/", "-Dorg.lwjgl.util.Debug=true", "-XX:+UnlockCommercialFeatures", "-XX:+FlightRecorder", "-Xmx384m", "-Xms128m")

autoCompilerPlugins := true

test in assembly := {}
