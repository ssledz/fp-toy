package monad.intro

import monad.intro.IdInstances.Id
import monad.intro.IdSyntax._
import monad.intro.MonadInstances._
import monad.intro.MonadSyntax._

object IdExample extends App {

  type ErrorOr[A] = Either[String, A]

  def sum[M[_] : Monad](xs: M[Int], ys: M[Int]): M[Int] = for {
    x <- xs
    y <- ys
  } yield x + y

  println(sum(1: Id[Int], 2: Id[Int]))
  println(sum(Id.pure(1), Id.pure(2)))
  println(sum(Option.pure(1), Option.pure(2)))
  println(sum(List.pure(1), List.pure(2)))
  println(sum(Either.pure(1): ErrorOr[Int], Either.pure(2): ErrorOr[Int]))

  println(4 + 1 |> (_ * 2))

  val x: String = null

  println(x ?? ("222"))

}
