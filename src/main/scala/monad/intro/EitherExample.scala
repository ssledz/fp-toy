package monad.intro

object EitherExample extends App {

  def parse(x: String): Either[String, Int] = try {
    Either.pure(x.toInt)
  } catch {
    case e: Exception => Left(s"Error during parsing $x: ${e.getMessage}")
  }

  def sum(a: String, b: String): Either[String, Int] = for {
    x <- parse(a)
    y <- parse(b)
  } yield x + y

  println(sum("1", "2"))
  println(sum("a", "2"))

  import MonadInstances._

  type StringOr[A] = Either[String, A]
  
  println(Monad[StringOr].pure(1))

}
