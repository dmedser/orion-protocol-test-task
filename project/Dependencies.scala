import sbt._

object Dependencies {

  object Versions {
    val cats = "2.6.0"
    val catsEffect = "3.1.0"
    val fs2 = "3.0.2"
    val scalatest = "3.2.8"
    val scalacheck = "1.15.3"
  }

  val cats = Seq("org.typelevel" %% "cats-core" % Versions.cats)

  val catsEffect = Seq("org.typelevel" %% "cats-effect" % Versions.catsEffect)

  val fs2 = Seq("co.fs2" %% "fs2-core" % Versions.fs2, "co.fs2" %% "fs2-io" % Versions.fs2)

  val scalatest = Seq("org.scalatest" %% "scalatest" % Versions.scalatest)

  val scalacheck = Seq("org.scalacheck" %% "scalacheck" % Versions.scalacheck)
}
