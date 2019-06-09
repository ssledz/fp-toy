package monad.intro

import monad.intro.Reader.{ask, asks}

object ReaderExample extends App {

  val length: Reader[String, Int] = for {
    content <- ask
  } yield content.length

  val mLength = Reader.local(length)(_ + "Prefix")

  val content = "12345"

  println(length.run(content))
  println(mLength.run(content))

  type Env = Map[String, Int]

  def lookup(key: String)(env: Env): Int = env.getOrElse(key, 0)

  val isCountCorrect: Reader[Env, Boolean] = for {
    count <- asks(lookup("count"))
    env <- ask
  } yield count == env.size

  val sampleEnv = Map("count" -> 3, "1" -> 1, "2" -> 2)
  println("is count correct: " + isCountCorrect.run(sampleEnv))

}
