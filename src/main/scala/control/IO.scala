package control

trait IO[A] {

  self =>

  def run: A

  def flatMap[B](f: A => IO[B]): IO[B] = IO(f(self.run).run)

  def map[B](f: A => B): IO[B] = flatMap(a => IO.pure(f(a)))

}

object IO {

  def apply[A](a: => A): IO[A] = new IO[A] {
    def run: A = a
  }

  def pure[A](a: A): IO[A] = IO(a)

}
