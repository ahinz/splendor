lazy val root = (project in file(".")).
  settings(
    name := "splendor",
    version := "1.0",
    scalaVersion := "2.11.6",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats" % "0.6.1",
      "com.typesafe.akka" %% "akka-http-experimental" % "2.4.9",
      "de.heikoseeberger" %% "akka-http-circe" % "1.9.0",
      "io.circe" %% "circe-core" % "0.4.1",
      "io.circe" %% "circe-generic" % "0.4.1",
      "io.circe" %% "circe-parser" % "0.4.1",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test",
      "com.typesafe.akka" %% "akka-http-testkit" % "2.4.9" % "test")
  )
