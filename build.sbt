organization := "org.gonsolo"

version := "0.1"

name := "borg"

scalaVersion := "2.13.12"

val chisel6Version = "6.5.0"

libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel" % chisel6Version,
  "edu.berkeley.cs" %% "rocketchip-6.0.0" % "1.6-6.0.0-1b9f43352-SNAPSHOT",
  "ch.epfl.scala" %% "bloop-config" % "1.5.5"
)
addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chisel6Version cross CrossVersion.full)

resolvers ++= Resolver.sonatypeOssRepos("snapshots")
resolvers ++= Resolver.sonatypeOssRepos("releases")
