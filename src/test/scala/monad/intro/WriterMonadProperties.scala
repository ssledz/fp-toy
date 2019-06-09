package monad.intro

import monad.intro.MonoidInstances._
import org.scalacheck.{Arbitrary, Gen, Properties}

object WriterMonadProperties extends Properties("WriterMonad") {

  type ListStringWriter[A] = Writer[List[String], A]

  private implicit val m: Monad[ListStringWriter] = MonadInstances.monadWriterInstance

  include(ListStringWriterMonadProperties)

  private case object ListStringWriterMonadProperties extends AbstractMonadProperties[String, Int, Int, ListStringWriter]("ListString") {

    import Generators.listArbitrary

    override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

    override implicit val arbM: Arbitrary[ListStringWriter[String]] = Generators.writerArbitrary[List[String], String]

    override implicit val arbF: Arbitrary[String => ListStringWriter[Int]] = Arbitrary(Gen.oneOf(Seq(x => Writer.pure[List[String], Int](x.length))))

    override implicit val arbG: Arbitrary[Int => ListStringWriter[Int]] = Arbitrary(Gen.oneOf(Seq(x => Writer.pure[List[String], Int](x + 1))))
  }

}


