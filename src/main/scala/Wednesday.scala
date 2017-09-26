import java.util.Date

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

    def rec(keys: List[Int], prev: ParMap[Int, GenSeq[(Int, Int)]] = Map().par): (ParSeq[(Int, Int)], ParMap[Int, GenSeq[(Int, Int)]]) = {

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
        complete -> nonComplete
      } else {
        rec(keys.union(newKeys), nonComplete)
      }
    }

    val keys0 = map.keys.toList.sorted

    val res = rec(keys0)
    val complete = res._1
    val nonComplete = res._2

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
    val maybeSolutions = results.par.map(_.seq.sorted).map(_.filter(map.contains)).distinct
    val solutions = maybeSolutions.filter(_.sum == targetSum)
    println(s"${new Date()} solutions (${solutions.length}) resolved")
    val out = solutions.flatMap { solution =>
      solution.map(key => map(key)).reduceLeft(combine)
    }
    out
  }

  def randomInput(n: Int): Table = {

    val r = new Random()

    def randomVector(length: Int): Row = {
      Vector.fill(length)(if (r.nextInt(100) >= 50) Some(r.nextInt(10000).toString) else None)
    }

    List.fill(n)(randomVector(12)).filter(_.exists(_.isDefined)).distinct
  }

  val random = randomInput(100000).par.distinct
  val input: Table = Json.extract[List[Vector[Option[String]]]]("input", MySerializer).par

  println(s"${new Date()} statred solving")
  val out = solve(input)
  println(s"${new Date()} solved")

  //println(input.mkString("\n"))
  println(out.length)

  Json.write[List[Vector[Option[String]]]](out.toList, "output", MySerializer)

}
