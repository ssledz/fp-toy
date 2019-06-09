package monad.intro

import monad.intro.MonadInstances._
import org.scalacheck.{Arbitrary, Gen}

case object OptionMonadProperties extends AbstractMonadProperties[String, Int, Int, Option]("OptionMonad") {

  override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

  override implicit val arbM: Arbitrary[Option[String]] = Generators.optionArbitrary

  override implicit val arbF: Arbitrary[String => Option[Int]] = Arbitrary(Gen.oneOf(Seq(x => Option.pure(x.length))))

  override implicit val arbG: Arbitrary[Int => Option[Int]] = Arbitrary(Gen.oneOf(Seq(x => Option.pure(x + 1))))
}
