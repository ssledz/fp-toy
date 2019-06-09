package monad.intro

object IdInstances {

  type Id[A] = A

  val id = new Monad[Id] {

    override def pure[A](x: A): Id[A] = x

    override def flatMap[A, B](xs: Id[A])(f: A => Id[B]): Id[B] = f(xs)
  }

  object Id {
    def pure[A](x: A): Id[A] = x
  }

}

object IdSyntax {

  implicit class IdOps[A](private val a: A) extends AnyVal {

    def |>[B](f: A => B): B = f(a)

    def ??(d: => A)(implicit ev: Null <:< A): A = if (a == ev(null)) d else a

  }

}
