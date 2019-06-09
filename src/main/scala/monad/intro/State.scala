package monad.intro

case class State[S, A](run: S => (S, A)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (s2, a) = run(s)
    f(a).run(s2)
  }

  def map[B](f: A => B): State[S, B] = flatMap(a => State.pure(f(a)))

  def runS(s: S): S = run(s)._1

  def runA(s: S): A = run(s)._2

}

object State {

  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] = xs match {
    case Nil => State(s => (s, List.empty))
    case h :: t => for {
      a <- h
      as <- sequence(t)
    } yield ::(a, as)
  }

}
