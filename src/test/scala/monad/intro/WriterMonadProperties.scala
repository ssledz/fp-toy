package monad.intro

import control.{Monad, MonadInstances, Writer}
import data.MonoidInstances._
import org.scalacheck.{Arbitrary, Gen, Properties}

object WriterMonadProperties extends Properties("WriterMonad") {

  type ListStringWriter[A] = Writer[data.List[String], A]

  private implicit val m: Monad[ListStringWriter] = MonadInstances.monadWriterInstance

  include(ListStringWriterMonadProperties)

  private case object ListStringWriterMonadProperties extends AbstractMonadProperties[String, Int, Int, ListStringWriter]("ListString") {

    import Generators.listArbitrary

    override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

    override implicit val arbM: Arbitrary[ListStringWriter[String]] = Generators.writerArbitrary[data.List[String], String]

    override implicit val arbF: Arbitrary[String => ListStringWriter[Int]] = Arbitrary(Gen.oneOf(Seq(x => Writer.pure[data.List[String], Int](x.length))))

    override implicit val arbG: Arbitrary[Int => ListStringWriter[Int]] = Arbitrary(Gen.oneOf(Seq(x => Writer.pure[data.List[String], Int](x + 1))))
  }

}


