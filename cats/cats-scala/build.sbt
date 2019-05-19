scalaVersion := "2.12.3"
libraryDependencies +=
  "org.typelevel" %% "cats-core" % "1.0.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.typelevel" %% "cats-testkit" % "1.1.0" % "test"
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-language:higherKinds"
)
