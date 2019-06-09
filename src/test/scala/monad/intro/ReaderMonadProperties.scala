package monad.intro

import monad.intro.MonadInstances._
import org.scalacheck.Gen.Parameters
import org.scalacheck.{Arbitrary, Gen, Properties}

object ReaderMonadProperties extends Properties("ReaderMonad") {

  type StringReader[A] = Reader[String, A]

  include(StringReaderMonadProperties)

  case object StringReaderMonadProperties extends AbstractMonadProperties[String, Int, Int, StringReader]("FixLeftString") {

    override implicit val arbA: Arbitrary[String] = Arbitrary(Gen.alphaStr)

    override implicit val arbM: Arbitrary[StringReader[String]] = Generators.readerArbitrary

    override implicit val arbF: Arbitrary[String => StringReader[Int]] = Arbitrary(Gen.oneOf(Seq(x => Reader.pure(x.length))))

    override implicit val arbG: Arbitrary[Int => StringReader[Int]] = Arbitrary(Gen.oneOf(Seq(x => Reader.pure(x + 1))))

    override def eq[T]: Eq[StringReader[T]] = (x: StringReader[T], y: StringReader[T]) => {
      import org.scalacheck.Prop.forAll
      val prop = forAll { s: String =>
        x.run(s) == y.run(s)
      }
      prop(Parameters.default.withSize(1000)).success
    }
  }

}


