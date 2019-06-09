package monad.intro

import monad.intro.IdInstances.Id
import monad.intro.MonadInstances._
import org.scalacheck.{Arbitrary, Gen}

case object IdMonadProperties extends AbstractMonadProperties[String, Int, Int, Id]("IdMonad") {

  override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

  override implicit val arbM: Arbitrary[Id[String]] = arbA

  override implicit val arbF: Arbitrary[String => Id[Int]] = Arbitrary(Gen.oneOf(Seq(x => Id.pure(x.length))))

  override implicit val arbG: Arbitrary[Int => Id[Int]] = Arbitrary(Gen.oneOf(Seq(x => Id.pure(x + 1))))
}
