lazy val root = (project in file(".")).
  settings(
    name := "splendor",
    version := "1.0",
    scalaVersion := "2.11.6",
    libraryDependencies += "org.typelevel" %% "cats" % "0.6.1"
  )
