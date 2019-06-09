package monad.intro

trait Monoid[M] {

  def combine(x: M, y: M): M

  def empty: M

}

object MonoidInstances {

  implicit def monoidListInstance[A]: Monoid[List[A]] = new Monoid[List[A]] {

    override def combine(x: List[A], y: List[A]): List[A] = x ::: y

    override def empty: List[A] = List.empty
  }

  implicit val monoidStrInstance: Monoid[String] = new Monoid[String] {

    override def combine(x: String, y: String): String = x + y

    override def empty: String = ""
  }

}
