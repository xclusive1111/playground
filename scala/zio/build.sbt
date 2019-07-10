name := "zio-playground"
version := "0.1"
scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "1.0.0-RC9",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-language:higherKinds"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.1")
