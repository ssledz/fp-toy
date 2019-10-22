package data

object OptionExample extends App {

  def sum(xs: data.Option[Int], ys: data.Option[Int]): data.Option[Int] = for {
    x <- xs
    y <- ys
  } yield x + y

  println(sum(data.Option.pure(1), data.Option.pure(2)))
  println(sum(data.Option.empty, data.Option.pure(2)))

}
