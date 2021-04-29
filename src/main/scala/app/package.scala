package object app {
  type StreamCompiler[F[_]] = fs2.Compiler[F, F]
}
