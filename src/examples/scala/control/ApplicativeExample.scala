package control

import control.ApplicativeInstances._
import control.ApplicativeSyntax._
import data.Option

object ApplicativeExample extends App {

  println((Option.pure(1), Option.pure(2)).mapN(_ + _))
  println((Option.empty[Int], Option.pure(2)).mapN(_ + _))

  val add: Int => Int => Int = ((x: Int, y: Int) => (x + y)).curried
  val liftedAdd: Option[Int => Int => Int] = Applicative[Option].pure(add)
  val fbc: Option[Int => Int] = Applicative[Option].ap(liftedAdd)(Option.pure(1))
  val fc = Applicative[Option].ap(fbc)(Option.pure(2))
  println(fc)

}
