package monad.intro

case class Reader[E, A](run: E => A) {

  def flatMap[B](f: A => Reader[E, B]): Reader[E, B] = Reader { e =>
    val a = run(e)
    f(a).run(e)
  }

  def map[B](f: A => B): Reader[E, B] = flatMap(a => Reader.pure(f(a)))

}

object Reader {

  def pure[E, A](a: A): Reader[E, A] = Reader(_ => a)

  def ask[E]: Reader[E, E] = Reader(identity)

  def asks[E, A](f: E => A): Reader[E, A] = Reader(e => f(e))

  def local[E, A](r: Reader[E, A])(f: E => E): Reader[E, A] = Reader(e => r.run(f(e)))
}
