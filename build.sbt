lazy val scala3Version = "3.8.3"
lazy val scala2Version = "2.13.18"
lazy val supportedScalaVersions = Seq(scala2Version, scala3Version)

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "com.nickrobison.inland"
ThisBuild / version := "0.0.1-SNAPSHOT"

lazy val commonSettings = Seq(
  crossScalaVersions := supportedScalaVersions,
  scalaVersion := scala3Version
)

lazy val allocator = (project in file("allocator"))
  .settings(commonSettings)
  .settings(
    name := "allocator",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % Test
  )

lazy val allocatorLaws = (project in file("allocator-laws"))
  .dependsOn(allocator)
  .settings(commonSettings)
  .settings(
    name := "allocator-laws",
    libraryDependencies += "org.typelevel" %% "discipline-core" % "1.7.0",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
  )

lazy val allocatorTests = (project in file("allocator-tests"))
  .dependsOn(allocator, allocatorLaws)
  .settings(commonSettings)
  .settings(
    name := "allocator-tests",
    libraryDependencies += "org.typelevel" %% "discipline-scalatest" % "2.3.0"
  )

lazy val root = (project in file("."))
  .aggregate(allocator, allocatorLaws, allocatorTests)
  .settings(
    name := "inland",
    idePackagePrefix := Some("com.nickrobison.inland")
  )
