package monad.intro

import control.Monad
import data.Eq
import data.Eq
import data.EqSyntax._
import org.scalacheck.Prop.forAll
import org.scalacheck._

abstract class AbstractMonadProperties[A, B, C, M[_]: Monad](name: String) extends Properties(name) {

  implicit val arbA: Arbitrary[A]

  implicit val arbM: Arbitrary[M[A]]

  implicit val arbF: Arbitrary[A => M[B]]

  implicit val arbG: Arbitrary[B => M[C]]

  private val monad = implicitly[Monad[M]]

  def eq[T]: Eq[M[T]] = Eq.fromEquals

  implicit val eqMa: Eq[M[A]] = eq[A]

  implicit val eqMb: Eq[M[B]] = eq[B]

  implicit val eqMc: Eq[M[C]] = eq[C]

  property("Left identity: return a >>= f ≡ f a") = forAll { (a: A, f: A => M[B]) =>
    (`return`(a) >>= f) === f(a)
  }

  property("Right identity: m >>= return ≡ m") = forAll { m: M[A] =>
    (m >>= `return`) === m
  }

  property("Associativity: (m >>= f) >>= g ≡ m >>= (\\x -> f x >>= g)") = forAll { (m: M[A], f: A => M[B], g: B => M[C]) =>
    ((m >>= f) >>= g) === (m >>= (x => f(x) >>= g))
  }

  private val `return`: A => M[A] = monad.pure _

  private implicit class MonadOps[D](m: M[D]) {
    def >>=[E](f: D => M[E]): M[E] = monad.flatMap(m)(f)
  }

}
