package data

import control.Monad
import control.MonadInstances._
import control.MonadSyntax._
import data.IdInstances.Id
import data.IdSyntax._

object IdExample extends App {

  type ErrorOr[A] = Either[String, A]

  def sum[M[_]: Monad](xs: M[Int], ys: M[Int]): M[Int] =
    for {
      x <- xs
      y <- ys
    } yield x + y

  println(sum(1: Id[Int], 2: Id[Int]))
  println(sum(Id.pure(1), Id.pure(2)))
  println(sum(data.Option.pure(1), data.Option.pure(2)))
  println(sum(data.List.pure(1), data.List.pure(2)))
  println(sum(Either.pure(1): ErrorOr[Int], Either.pure(2): ErrorOr[Int]))

  println(4 + 1 |> (_ * 2))

  val x: String = null

  println(x ?? ("222"))

}
