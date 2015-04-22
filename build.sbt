name := "semantic-anomaly-detection"

version := "1.0"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.1",
  "org.json4s" %% "json4s-native" % "3.2.11",
  "org.specs2" %% "specs2-core" % "2.4" % "test",
  "org.typelevel" %% "scalaz-specs2" % "0.3.0" % "test"
)