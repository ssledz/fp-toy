package monad.intro

case class Writer[L, A](run: (L, A)) {

  def flatMap[B](f: A => Writer[L, B])(implicit m: Monoid[L]): Writer[L, B] = Writer[L, B] {
    val (l, a) = run
    val (ll, b) = f(a).run
    (m.combine(l, ll), b)
  }

  def map[B](f: A => B)(implicit m: Monoid[L]): Writer[L, B] = flatMap(a => Writer.pure(f(a)))

}

object Writer {

  def pure[L, A](a: A)(implicit m: Monoid[L]): Writer[L, A] = Writer((m.empty, a))

  def tell[L](l: L): Writer[L, Unit] = Writer((l, ()))

}