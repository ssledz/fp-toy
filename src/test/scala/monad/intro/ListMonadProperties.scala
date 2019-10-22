package monad.intro

import control.MonadInstances._
import org.scalacheck.{Arbitrary, Gen}

case object ListMonadProperties extends AbstractMonadProperties[String, Int, Int, data.List]("ListMonad") {

  override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

  override implicit val arbM: Arbitrary[data.List[String]] = Generators.listArbitrary[String]

  override implicit val arbF: Arbitrary[String => data.List[Int]] = Arbitrary(Gen.oneOf(Seq(x => data.List(x.length))))

  override implicit val arbG: Arbitrary[Int => data.List[Int]] = Arbitrary(Gen.oneOf(Seq(x => data.List(x - 1, x, x + 1))))
}
