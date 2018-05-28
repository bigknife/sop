/**
  * The definition of *ALL Dependencies*
  */
import sbt._, Keys._

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
      val catsNormal    = "1.0.0-RC2"
      val catsEffect    = "0.5"
      val catsMtl       = "0.0.2"
      val catsMouse     = "0.12"
      val catsMachinist = "0.6.2"
      val scalameta     = "1.8.0"
    }
    lazy val cats = {
      Seq("cats-core", "cats-macros", "cats-kernel", "cats-core", "cats-free" /*,
          "cats-effect",
          "cats-mtl-core",
          "mouse"*/ )
        .map({
          case x if x == "cats-effect"   ⇒ Dpd("org.typelevel", x, versions.catsEffect)
          case x if x == "cats-mtl-core" ⇒ Dpd("org.typelevel", x, versions.catsMtl)
          case x if x == "mouse"         ⇒ Dpd("org.typelevel", x, versions.catsMouse)
          case x                         ⇒ Dpd("org.typelevel", x, versions.catsNormal)
        })
        .map({
          case x ⇒ x.libraryDependencies
        })
    }

    lazy val scalameta = Dpd("org.scalameta", "scalameta", versions.scalameta).libraryDependencies

    private lazy val catsOverrides = Seq("cats-core", "machinist")
      .map({
        case x if x == "machinist" ⇒ Dpd("org.typelevel", x, versions.catsMachinist)
        case x                     ⇒ Dpd("org.typelevel", x, versions.catsNormal)
      })
      .map(_.libraryDependencies)

    lazy val overrides = catsOverrides
  }

  // resolvers
  object resolver {
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

    lazy val local = "local maven repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

    object credential {
      lazy val weihui  = Credentials(Path.userHome / ".dev" / "weihui.credentials")
      lazy val barcsys = Credentials(Path.userHome / ".dev" / "barcsys.credentials")
    }
  }
}