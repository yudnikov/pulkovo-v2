import Implicits.OptVectorToInt
import Implicits.IntToBoolVector

import scala.annotation.tailrec

object Wednesday extends App {

  type LVOS = List[Vector[Option[String]]]
  type Stair[T] = (T, List[T])

  @tailrec
  def getStairs[T](list: List[T], acc: List[Stair[T]] = Nil): List[Stair[T]] = {
    if (list.nonEmpty)
      getStairs(list.tail, (list.head -> list.tail) :: acc)
    else
      acc
  }

  def isValidPair(t: (Int, Int), length: Int): Boolean = {
    val ab = t._1.toBoolVector(length) zip t._2.toBoolVector(length)
    !ab.exists(bb => bb._1 & bb._2)
  }

  def solve(input: LVOS): LVOS = {

    val length = input.head.length
    val targetSum = Math.pow(2, length).toInt - 1
    val map = input.par.groupBy(_.toInt)

    def rec(keys: List[Int], prev: Map[Int, Any] = Map(), f: () => LVOS = () => {
      println(s"executing")
      Nil
    }): LVOS = {
      val stairs = getStairs(map.keys.toList.sorted)
      val split = stairs.flatMap { stair =>
        stair._2.map { r =>
          stair._1 -> r
        }
      } filter { t =>
        //isValidPair(t, length) &&
          //t._1 < t._2 &&
          t._1 + t._2 <= targetSum
      } partition { t =>
        t._1 + t._2 == targetSum
      }
      val complete = split._1
      val nonComplete = split._2.groupBy(t => t._1 + t._2)
      val newKeys = nonComplete.keys.toList.diff(keys).sorted
      //if (nonComplete == prev) {
      if (false) {
        f()
      } else {
        rec(keys.union(newKeys), nonComplete, () => f().union(Nil))
      }
    }
    rec(map.keys.toList)
  }

  val input: LVOS = Json.extract[LVOS]("input2", MySerializer)

  solve(input)

}
