name := "hoogle-sheets"
scalaVersion := "2.12.8"
organization := "de.dfki"

scalaSource in Compile := baseDirectory.value / "scala-server"
javaSource in Compile  := baseDirectory.value / "scala-server"

unmanagedSourceDirectories in Compile += baseDirectory.value / "scala-shared"

resourceDirectory in Compile := baseDirectory.value / "scala-client" / "assets"

libraryDependencies ++= Seq(  
  "com.typesafe.akka" %% "akka-http" % "10.1.8",
  "com.typesafe.akka" %% "akka-stream" % "2.5.19",
  "io.circe" %% "circe-core" % "0.10.0",
  "io.circe" %% "circe-generic" % "0.10.0",
  "io.circe" %% "circe-parser" % "0.10.0")