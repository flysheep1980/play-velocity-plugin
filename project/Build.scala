/*
 * Copyright (C) 2013 FURYU CORPORATION
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Includes Apache Velocity
 *
 *   http://velocity.apache.org/
 *
 * Copyright (C) 2000-2007 The Apache Software Foundation
 */
import sbt._
import Keys._
import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform._
import play.Project._

object ApplicationBuild extends Build {

  val appOrganization	= "jp.furyu"
  val appName         = "play-velocity-plugin"
  val appVersion      = "1.2-SNAPSHOT"
  val appScalaVersion = "2.10.0"
  val appScalaCrossVersions = Seq("2.10.0", "2.9.1")

  lazy val scalaRiformSettings = ScalariformKeys.preferences := FormattingPreferences().setPreference(IndentWithTabs, false).setPreference(DoubleIndentClassDeclaration, true).setPreference(PreserveDanglingCloseParenthesis, true)

  lazy val root = Project("root", base = file("."))
    .dependsOn(plugin)
    .aggregate(scalaSample)

  lazy val plugin = Project(appName, base = file("plugin")).settings(Defaults.defaultSettings: _*).settings(
    scalaVersion := appScalaVersion,
    crossScalaVersions := appScalaCrossVersions,
    resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
    libraryDependencies <+= scalaVersion(v => {
      v match {
        case "2.10.0" => "play" % "play_2.10" % "[2.1,)"
        case "2.9.1" => "play" % "play_2.9.1" % "[2.0,)"
      }
    }),
    libraryDependencies <+= scalaVersion(v => {
      v match {
        case "2.10.0" => "org.specs2" %% "specs2" % "1.14" % "test"
        case "2.9.1" => "org.specs2" %% "specs2" % "1.12.3" % "test"
      }
    }),
    libraryDependencies ++= Seq(
      "org.apache.velocity" % "velocity" % "[1.7,)",
      "play" %% "play-test" % "[2.0,)" % "test",
      "commons-lang" % "commons-lang" % "2.6"
    ),
    organization := appOrganization,
    version := appVersion,
    publishMavenStyle := true,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) {
        Some("snapshots" at nexus + "content/repositories/snapshots")
      } else {
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      }
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <url>https://github.com/Furyu/play-velocity-plugin</url>
      <licenses>
        <license>
          <name>Apache License, Version 2.0</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:Furyu/play-velocity-plugin.git</url>
        <connection>scm:git:git@github.com:Furyu/play-velocity-plugin.git</connection>
      </scm>
      <developers>
        <developer>
          <id>flysheep1980</id>
          <name>flysheep1980</name>
          <url>https://github.com/flysheep1980</url>
        </developer>
      </developers>
    )
  ).settings(scalariformSettings: _*).settings(scalaRiformSettings)
  
  lazy val scalaSample = play.Project("scala-sample", path = file("samples/scala")).settings( 
    scalaVersion := appScalaVersion
  ).settings(com.typesafe.sbt.SbtScalariform.scalariformSettings: _*).settings(scalaRiformSettings).dependsOn(plugin)

}
