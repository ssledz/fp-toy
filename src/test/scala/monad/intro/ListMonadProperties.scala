package monad.intro

import monad.intro.MonadInstances._
import org.scalacheck.{Arbitrary, Gen}

case object ListMonadProperties extends AbstractMonadProperties[String, Int, Int, List]("ListMonad") {

  override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

  override implicit val arbM: Arbitrary[List[String]] = Generators.listArbitrary[String]

  override implicit val arbF: Arbitrary[String => List[Int]] = Arbitrary(Gen.oneOf(Seq(x => List(x.length))))

  override implicit val arbG: Arbitrary[Int => List[Int]] = Arbitrary(Gen.oneOf(Seq(x => List(x - 1, x, x + 1))))
}
