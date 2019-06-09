package monad.intro

import monad.intro.MonadInstances._
import org.scalacheck.{Arbitrary, Gen, Properties}

object EitherMonadProperties extends Properties("EitherMonad") {

  type StringEither[A] = Either[String, A]

  include(StringEitherMonadProperties)

  case object StringEitherMonadProperties extends AbstractMonadProperties[String, Int, Int, StringEither]("FixLeftString") {

    override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

    override implicit val arbM: Arbitrary[StringEither[String]] = Generators.eitherArbitrary

    override implicit val arbF: Arbitrary[String => StringEither[Int]] = Arbitrary(Gen.oneOf(Seq(x => Either.pure(x.length))))

    override implicit val arbG: Arbitrary[Int => StringEither[Int]] = Arbitrary(Gen.oneOf(Seq(x => Either.pure(x + 1))))
  }

}


