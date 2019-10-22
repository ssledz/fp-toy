package data

import control.Monad

object EitherExample extends App {

  def parse(x: String): data.Either[String, Int] = try {
    data.Either.pure(x.toInt)
  } catch {
    case e: Exception => Left(s"Error during parsing $x: ${e.getMessage}")
  }

  def sum(a: String, b: String): data.Either[String, Int] = for {
    x <- parse(a)
    y <- parse(b)
  } yield x + y

  println(sum("1", "2"))
  println(sum("a", "2"))

  import control.MonadInstances._

  type StringOr[A] = data.Either[String, A]

  println(Monad[StringOr].pure(1))

}
