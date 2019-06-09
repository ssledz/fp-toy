package monad.intro

object TryExample extends App {

  def sum(a: String, b: String): Try[Int] = for {
    x <- Try.pure(a.toInt)
    y <- Try.pure(b.toInt)
  } yield x + y

  println(sum("1", "2"))
  println(sum("a", "2"))

}
