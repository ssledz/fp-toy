package monad.intro

import control.MonadInstances._
import org.scalacheck.{Arbitrary, Gen}

case object OptionMonadProperties extends AbstractMonadProperties[String, Int, Int, data.Option]("OptionMonad") {

  override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

  override implicit val arbM: Arbitrary[data.Option[String]] = Generators.optionArbitrary

  override implicit val arbF: Arbitrary[String => data.Option[Int]] = Arbitrary(Gen.oneOf(Seq(x => data.Option.pure(x.length))))

  override implicit val arbG: Arbitrary[Int => data.Option[Int]] = Arbitrary(Gen.oneOf(Seq(x => data.Option.pure(x + 1))))
}
