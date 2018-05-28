import Common._
import Common.prj._

lazy val core = scalametaDependedOn {
  catsDependedOn {
    prj("core", file("core")).settings(
    )
  }
}

lazy val effect = scalametaDependedOn {
  prj("effect", file("effect"))
    .dependsOn(core)
    .settings(
      publishArtifact in (Compile, packageDoc) := false,
      publishArtifact in packageDoc := false,
      sources in (Compile, doc) := Seq.empty
    )
}