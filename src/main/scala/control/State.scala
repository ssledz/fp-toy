package control

import data._

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

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def sequence[S, A](xs: data.List[State[S, A]]): State[S, data.List[A]] = xs match {
    case Nil => State(s => (s, data.List.empty))
    case h :: t =>
      for {
        a <- h
        as <- sequence(t)
      } yield data.::(a, as)
  }

}
