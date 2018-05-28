/**
  * The definition of *ALL Dependencies*
  */
import sbt._, Keys._
import bintray.BintrayKeys._

object Dependencies {
  case class Dpd(groupId: String,
                 artifactId: String,
                 revision: String,
                 autoScalaVersion: Boolean = true,
                 configuration: String = "compile") {
    def libraryDependencies =
      if (autoScalaVersion) groupId %% artifactId % revision % configuration
      else groupId                  % artifactId  % revision % configuration
  }

  // all dependencies
  object all {
    object versions {
      val catsNormal    = "1.0.1"
      val catsEffect    = "1.0.0-RC"
      val scalameta     = "1.8.0"
      val scalatest = "3.0.5"
    }
    lazy val cats = {
      Seq("cats-core", "cats-free")
        .map({
          case x if x == "cats-effect"   ⇒ Dpd("org.typelevel", x, versions.catsEffect)
          case x                         ⇒ Dpd("org.typelevel", x, versions.catsNormal)
        })
        .map(_.libraryDependencies)
    }
    lazy val scalameta = Dpd("org.scalameta", "scalameta", versions.scalameta).libraryDependencies

    lazy val scalatest = Seq(
      Dpd("org.scalactic", "scalactic", versions.scalatest),
      Dpd("org.scalatest", "scalatest", versions.scalatest, autoScalaVersion = true, configuration = "test")
    ).map(_.libraryDependencies)
  }

  // resolvers
  object resolver {
    /*
    object weihui {
      private val nexus = "http://nexus.weihui.com:8081"
      lazy val snapshot = "weihui_snapshot" at nexus + "/content/repositories/snapshots"
      lazy val release  = "weihui_release" at nexus + "/content/repositories/releases"
    }

    object barcsys {
      private val nexus = "https://nexus.barcsys.com"
      lazy val snapshot = "barcsys_snapshot" at nexus + "/content/repositories/snapshots"
      lazy val release  = "barcsys_release" at nexus + "/content/repositories/releases"
    }
    */

    lazy val local = "local maven repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

    object credential {
      lazy val jfrog  = Credentials(Path.userHome / ".dev" / "jfrog.credentials")
    }
  }
}