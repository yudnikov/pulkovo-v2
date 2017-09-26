import Implicits.OptVectorToInt
import Implicits.IntToBoolVector

import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.collection.parallel.ParMap
import scala.collection.parallel.ParSeq
import scala.util.Random

object Wednesday extends App {

  type Row = Vector[Option[String]]
  type Table = GenSeq[Row]
  type Stair[T] = (T, List[T])

  @tailrec
  def getStairs[T](list: List[T], acc: List[Stair[T]] = Nil): List[Stair[T]] = {
    if (list.nonEmpty)
      getStairs(list.tail, (list.head -> list.tail) :: acc)
    else {
      println(s"got stairs")
      acc
    }
  }

  def isValidPair(t: (Int, Int), length: Int): Boolean = {
    val ab = t._1.toBoolVector(length) zip t._2.toBoolVector(length)
    !ab.exists(bb => bb._1 & bb._2)
  }

  def join(a: Row, b: Row): Row = {
    a zip b map { ab =>
      if (ab._1.isDefined) ab._1 else ab._2
    }
  }

  def combine(a: Table, b: Table): Table = {
    for {
      ai <- a
      bi <- b
    } yield join(ai, bi)
  }

  def solve(input: Table): Table = {

    val length = input.head.length
    val targetSum = Math.pow(2, length).toInt - 1
    val map = input.par.groupBy(_.toInt)

    def rec(keys: List[Int], prev: ParMap[Int, GenSeq[(Int, Int)]] = Map().par): Table = {

      val split = getStairs(keys).par.flatMap { stair =>
        stair._2.map { r =>
          stair._1 -> r
        }
      } filter { t =>
        isValidPair(t, length) &&
          t._1 < t._2 &&
          t._1 + t._2 <= targetSum
      } partition { t =>
        t._1 + t._2 == targetSum
      }

      val complete = split._1
      val nonComplete = {
        val current = split._2.groupBy(t => t._1 + t._2)
        if (prev.isEmpty) {
          current
        } else {
          split._2.collect {
            case t if prev.contains(t._1 + t._2) =>
              val sum = t._1 + t._2
              sum -> prev(sum)
            case t =>
              val sum = t._1 + t._2
              sum -> current(sum)
          }.toMap
        }
      }

      val newKeys = nonComplete.keys.toList.diff(keys).sorted

      if (nonComplete == prev) {

        val keys0 = map.keys.toList.sorted

        def resolve(list: GenSeq[(Int, Int)]): GenSeq[GenSeq[Int]] = {
          list.collect {
            case t if nonComplete.contains(t._1) && !keys0.contains(t._1) && nonComplete.contains(t._2) && !keys0.contains(t._2) =>
              resolve(nonComplete(t._1)).flatten.union(resolve(nonComplete(t._2)).flatten).distinct
            case t if nonComplete.contains(t._1) && !keys0.contains(t._1) && keys0.contains(t._2) =>
              resolve(nonComplete(t._1)).flatten.union(List(t._2)).distinct
            case t if nonComplete.contains(t._2) && !keys0.contains(t._2) && keys0.contains(t._1) =>
              resolve(nonComplete(t._2)).flatten.union(List(t._1)).distinct
            case t if keys0.contains(t._2) && keys0.contains(t._1) =>
              List(t._1, t._2).distinct
          }
        }

        val results = resolve(complete)
        val maybeSolutions = results.map(_.seq.sorted).map(_.filter(map.contains)).distinct
        val solutions = maybeSolutions.filter(_.sum == targetSum)
        val out = solutions.par.flatMap { solution =>
          solution.map(key => map(key)).reduceLeft(combine)
        }
        out
      } else {
        println(s"+ iteration")
        rec(keys.union(newKeys), nonComplete)
      }
    }

    rec(map.keys.toList.sorted)
  }

  def randomInput(n: Int): Table = {

    val r = new Random()

    def randomVector(length: Int): Row = {
      Vector.fill(length)(if (r.nextInt(100) >= 50) Some(r.nextInt(1000).toString) else None)
    }

    List.fill(n)(randomVector(12)).filter(_.exists(_.isDefined)).distinct
  }

  val input: Table = randomInput(100).par

  //val input: LVOS = Json.extract[LVOS]("input", MySerializer)

  println(input.mkString("\n"))
  println(solve(input).length)
  //println(solve(input).mkString("\n"))

}
