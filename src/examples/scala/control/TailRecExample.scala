package control

object TailRecExample extends App {

  val f: Int => Int = x => x
  val mf: Int => TailRec[Int] = x => TailRec.unit(x)

  val xf = List.fill(10000)(f).foldLeft(f)(_.andThen(_))

  val xmf: Int => TailRec[Int] = List.fill(10000)(mf).foldLeft(mf) { (acc, a) => x =>
    TailRec.suspend(acc(x).flatMap(a))
  }

  //  println(xf(0)) // stack overflow
  println(TailRec.run(xmf(0)))
  println(TailRec.run(xmf(10)))

}
