package monad.intro

import scala.io.StdIn
import MonadInstances._

object IOExample extends App {

  val askForAge: IO[Unit] = IO {
    println("What is your age ?")
  }

  val readAge: IO[String] = IO {
    StdIn.readLine()
  }

  def printAge(maybeAge: Try[Int]): IO[Unit] = IO {
    maybeAge match {
      case Success(age) => println(s"Your age is $age")
      case Failure(e) => println(s"Can't parse your age: $e")
    }
  }

  val program: IO[Try[Int]] = for {
    _ <- askForAge
    age <- readAge.map(x => Try(x.toInt))
    _ <- printAge(age)
  } yield age

  val xx = Monad.replicateM(4)(askForAge)

  xx.run
}
