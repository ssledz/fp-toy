package monad.intro

object ListExample extends App {

  def sum(xs: List[Int], ys: List[Int]): List[Int] = for {
    x <- xs
    y <- ys
  } yield x + y

  println(sum(List(1, 2, 3, 5, 7), List(4, 6, 8)))


}
