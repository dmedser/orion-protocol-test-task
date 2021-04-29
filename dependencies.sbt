libraryDependencies ++= {

  import Dependencies._

  val compile = cats ++ catsEffect ++ fs2

  val test = scalatest ++ scalacheck

  compile ++ test.map(_ % Test)
}