import Implicits._

import scala.annotation.tailrec

object Wednesday extends App {

  type LVOS = List[Vector[Option[String]]]
  type Stair[T] = (T, List[T])

  val input: LVOS = Json.extract[LVOS]("input2", MySerializer)

  def solve(input: LVOS): LVOS = {
    @tailrec
    def getStairs[T](list: List[T], acc: List[Stair[T]] = Nil): List[Stair[T]] = {
      if (list.nonEmpty)
        getStairs(list.tail, (list.head -> list.tail) :: acc)
      else
        acc
    }
    val map = input.par.groupBy(_.toInt)
    def rec(keys: List[Int], prev: List[Int] = Nil, f: () => LVOS = () => {
      println(s"executing")
      Nil
    }): LVOS = {
      val stairs = getStairs(map.keys.toList)
      if (keys == prev) {
        f()
      } else {
        rec(keys, keys, () => f().union(Nil))
      }
    }
    rec(map.keys.toList)
  }

  solve(input)

}
