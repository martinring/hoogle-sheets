enablePlugins(ScalaJSPlugin)

name := "hoogle-sheets-client"
scalaVersion := "2.12.1"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
libraryDependencies += "io.circe" %%% "circe-core" % "0.7.0"
libraryDependencies += "io.circe" %%% "circe-generic" % "0.7.0"
libraryDependencies += "io.circe" %%% "circe-parser" % "0.7.0"

unmanagedSourceDirectories in Compile += baseDirectory.value / ".." / "scala-shared"

resourceDirectory in Compile := baseDirectory.value / "scala-client" / "assets"

artifactPath in (Compile, fastOptJS) := file("assets/client.js")

scalaJSUseMainModuleInitializer := true