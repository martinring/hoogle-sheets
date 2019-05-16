name := "hoogle-sheets"
scalaVersion := "2.12.1"

scalaSource in Compile := baseDirectory.value / "scala-server"
javaSource in Compile  := baseDirectory.value / "scala-server"

unmanagedSourceDirectories in Compile += baseDirectory.value / "scala-shared"

resourceDirectory in Compile := baseDirectory.value / "scala-client" / "assets"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.0.6"
libraryDependencies += "io.circe" %% "circe-core" % "0.7.0"
libraryDependencies += "io.circe" %% "circe-generic" % "0.7.0"
libraryDependencies += "io.circe" %% "circe-parser" % "0.7.0"