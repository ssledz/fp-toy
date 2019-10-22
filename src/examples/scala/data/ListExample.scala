package data

object ListExample extends App {

  def sum(xs: data.List[Int], ys: data.List[Int]): data.List[Int] = for {
    x <- xs
    y <- ys
  } yield x + y

  println(sum(data.List(1, 2, 3, 5, 7), data.List(4, 6, 8)))


}
