package monad.intro


object OptionExample extends App {

  def sum(xs: Option[Int], ys: Option[Int]): Option[Int] = for {
    x <- xs
    y <- ys
  } yield x + y

  println(sum(Option.pure(1), Option.pure(2)))
  println(sum(Option.empty, Option.pure(2)))

}
