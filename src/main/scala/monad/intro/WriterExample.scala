package monad.intro

import monad.intro.MonoidInstances._

object WriterExample extends App {

  type Logger[A] = Writer[List[String], A]

  def sum(xs: Logger[Int], ys: Logger[Int]): Logger[Int] = for {
    x <- xs
    y <- ys
    _ <- Writer.tell(List(s"$x + $y"))
  } yield x + y

  def sub(xs: Logger[Int], ys: Logger[Int]): Logger[Int] = for {
    x <- xs
    y <- ys
    _ <- Writer.tell(List(s"$x - $y"))
  } yield x - y

  val res = sum(Writer.pure(1), Writer.pure(2))

  println(res)
  println(sub(res, Writer.pure(7)))

}
