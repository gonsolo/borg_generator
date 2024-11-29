ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1"
ThisBuild / organization     := "org.gonsolo"

val chisel6Version = "6.6.0"

lazy val root = (project in file("."))
  .settings(
    name := "borg",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chisel6Version,
      "edu.berkeley.cs" %% "rocketchip-6.0.0" % "1.6-6.0.0-1b9f43352-SNAPSHOT",
      "ch.epfl.scala" %% "bloop-config" % "1.5.5"
    ),
    scalacOptions ++= Seq(
      "-deprecation",
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chisel6Version cross CrossVersion.full),
    resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
    resolvers ++= Resolver.sonatypeOssRepos("releases")
)

