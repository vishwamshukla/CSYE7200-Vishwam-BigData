case class Pendulum(length1: List[Double], time1: List[Double]) {
  def map2[R, U](other: Pendulum)(g: (Double, Double) => R): List[R] = {
    for {
      length <- length1
      time <- time1
    } yield g(length, time)
  }
}
val t = List(0.5,0.75,1,1.25,1.5,2,5)
val u = List(3.821,4.800,5.357,6.222,6.688,7.644,12.264)
def gravity(length:Double, time:Double):Double ={
  val g = 4*Math.pow(Math.PI, 2)*Math.pow(length, -1)/Math.pow(time, 2)
  g
}
val data = Pendulum(t, u)
val g: List[Double] = data.map2(data)(gravity)
val mean:Double = g.sum/g.length
println(g)
println(mean)

//
//def map2[R, U](other: M[U])(g: (T, U) => R): M[R] = {
//  for {
//    t <- this
//    u <- other
//  } yield g(t, u)
//}

//reference - Some from PowerPoint and https://medium.com/@mallikakulkarni/functional-programming-in-scala-2-the-map-function-f9b9ee17d495, https://www.scala-lang.org/api/2.13.13/scala/collection/immutable/Map$$Map2.html