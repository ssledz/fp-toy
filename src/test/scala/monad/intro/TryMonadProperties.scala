package monad.intro

import monad.intro.MonadInstances._
import org.scalacheck.{Arbitrary, Gen}

case object TryMonadProperties extends AbstractMonadProperties[String, Int, Int, Try]("TryMonad") {

  override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

  override implicit val arbM: Arbitrary[Try[String]] = Generators.tryArbitrary

  override implicit val arbF: Arbitrary[String => Try[Int]] = Arbitrary(Gen.oneOf(Seq(x => Try.pure(x.length))))

  override implicit val arbG: Arbitrary[Int => Try[Int]] = Arbitrary(Gen.oneOf(Seq(x => Try.pure(x + 1))))
}
