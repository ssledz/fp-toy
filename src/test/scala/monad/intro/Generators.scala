package monad.intro

import org.scalacheck.Arbitrary
import org.scalacheck.Gen.{const, frequency}

trait Generators {
  implicit def listArbitrary[A](implicit arb: Arbitrary[scala.List[A]]): Arbitrary[List[A]] = Arbitrary(
    arb.arbitrary.map(l => l.foldLeft(List.empty[A])((xs, x) => ::(x, xs)))
  )

  implicit def optionArbitrary[A](implicit arb: Arbitrary[A]): Arbitrary[Option[A]] = Arbitrary(
    frequency(3 -> const(None), 7 -> arb.arbitrary.map(Some(_)))
  )

  implicit def tryArbitrary[A](implicit arb: Arbitrary[A]): Arbitrary[Try[A]] = Arbitrary(
    frequency(3 -> const(Failure(new Exception)), 7 -> arb.arbitrary.map(Success(_)))
  )

  implicit def eitherArbitrary[A, B](implicit arbl: Arbitrary[A], arbr: Arbitrary[B]): Arbitrary[Either[A, B]] = Arbitrary(
    frequency(3 -> arbl.arbitrary.map(Left(_)), 7 -> arbr.arbitrary.map(Right(_)))
  )

  implicit def readerArbitrary[E, A](implicit arb: Arbitrary[A]): Arbitrary[Reader[E, A]] = Arbitrary(
    for {
      a <- arb.arbitrary
    } yield Reader.pure(a)
  )

  implicit def writerArbitrary[L, A](implicit arbl: Arbitrary[L], arba: Arbitrary[A]): Arbitrary[Writer[L, A]] = Arbitrary(
    for {
      l <- arbl.arbitrary
      a <- arba.arbitrary
    } yield Writer((l, a))
  )

  implicit def stateArbitrary[S, A](implicit arba: Arbitrary[A]): Arbitrary[State[S, A]] = Arbitrary(
    for {
      a <- arba.arbitrary
    } yield State.pure(a)
  )
}

object Generators extends Generators
