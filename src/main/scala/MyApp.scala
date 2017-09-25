import scala.util.Random
import Implicits._

import scala.annotation.tailrec

object MyApp extends App {

  val length = 4

  def randomInput(n: Int): List[Vector[Option[String]]] = {

    val r = new Random()

    def randomVector(length: Int): Vector[Option[String]] = {
      Vector.fill(length)(if (r.nextInt(100) >= 50) Some(r.nextInt(100).toString) else None)
    }

    List.fill(n)(randomVector(length)).filter(_.exists(_.isDefined)).distinct
  }

  val input: List[Vector[Option[String]]] = randomInput(4)

  val mapped = input.distinct.map(row => row.toInt -> row)
  val grouped0 = mapped.groupBy(_._1).map(t => t._1 -> t._2.map(_._2))
  val targetSum = Math.pow(2, length).toInt - 1
  val keys0 = grouped0.keys.toList

  type Stair[T] = (T, List[T])

  @tailrec
  def getStairs[T](list: List[T], acc: List[(T, List[T])] = Nil): List[Stair[T]] = {
    if (list.nonEmpty)
      getStairs(list.tail, (list.head -> list.tail) :: acc)
    else
      acc
  }

  @tailrec
  def solve(keys: List[Int], solved: List[(Int, Int)] = Nil, prev: List[Int] = Nil): (List[(Int, Int)], List[(Int, Int)]) = {
    val stairs = getStairs(keys.filter(_ != targetSum))
    val split = stairs.flatMap { stair =>
      stair._2.map { r =>
        stair._1 -> r
      }
    } filter { t =>
      t._1 + t._2 <= targetSum
    } partition { t =>
      t._1 + t._2 == targetSum
    }

    val newKeys = split._2.map(t => t._1 + t._2).distinct
    val allKeys = keys.union(newKeys).distinct

    if (prev == newKeys)
      solved -> split._2
    else
      solve(allKeys, solved.union(split._1).distinct, newKeys)
  }

  println(input.mkString("\n"))

  val res = solve(keys0)
  //println(res)

  val complete = res._1
  val nonComplete = res._2

  val nonCompleteGrouped = nonComplete.groupBy(t => t._1 + t._2)

  val resolveSplit = complete.partition { t =>
    keys0.contains(t._1) && keys0.contains(t._2)
  }

  val resolved = resolveSplit._1
  val nonResolved = resolveSplit._2

  nonResolved.map {
    case _ =>
  }

  def join(a: Vector[Option[String]], b: Vector[Option[String]]): Vector[Option[String]] = {
    a zip b map { ab =>
      if (ab._1.isDefined) ab._1 else ab._2
    }
  }

  def combine(a: List[Vector[Option[String]]], b: List[Vector[Option[String]]]): List[Vector[Option[String]]] = {
    for {
      ai <- a
      bi <- b
    } yield join(ai, bi)
  }

  def resolve(x: List[(Int, Int)]): List[Vector[Option[String]]] = {
    println(s"resolving: $x")
    x.flatMap {
      case t if nonCompleteGrouped.contains(t._1) && nonCompleteGrouped.contains(t._2) =>
        combine(resolve(nonCompleteGrouped(t._1)), resolve(nonCompleteGrouped(t._2)))
      case t if nonCompleteGrouped.contains(t._1) && grouped0.contains(t._2) =>
        combine(resolve(nonCompleteGrouped(t._1)), grouped0(t._2))
      case t if nonCompleteGrouped.contains(t._2) && grouped0.contains(t._1) =>
        combine(resolve(nonCompleteGrouped(t._2)), grouped0(t._1))
      case t if grouped0.contains(t._1) && grouped0.contains(t._2) =>
        combine(grouped0(t._1), grouped0(t._2))
    }
  }

  val out = resolve(complete)

  /*val iter = split._2.map {
    case t if map.contains(t._1) && grouped.contains(t._2) =>
      map(t._1) -> grouped(t._2)
    case t if map.contains(t._2) && grouped.contains(t._1) =>
      map(t._2) -> grouped(t._1)
    case t if map.contains(t._1) && map.contains(t._2) =>
      map(t._1) -> map(t._2)
    case t if grouped.contains(t._1) && grouped.contains(t._2) =>
      grouped(t._1) -> grouped(t._2)
  }*/

  /*
  val iter2 = iter.map { i =>
    i._1.collect {
      case t if map.contains(t._1) && grouped.contains(t._2) =>
        map(t._1) -> grouped(t._2)
      case t if map.contains(t._2) && grouped.contains(t._1) =>
        map(t._2) -> grouped(t._1)
    }
  }*/

  //v
  //al iter = split._2.map(t => if (keys0.contains(t._1)) keys0(t._1) else ))
  //val y = res._2.filter(t => keys0.contains(t._1) && keys0.contains(t._2))

  println(out.length)

  //println(y)

}

object Implicits {

  implicit class OptVectorToInt(vector: Vector[Option[_]]) {

    def toInt: Int = Integer.parseInt(vector.map {
      case opt if opt.isDefined => '1'
      case _ => '0'
    }.mkString, 2)

  }

  implicit class IntToBoolVector(int: Int) {

    def toBoolVector(implicit length: Int): Vector[Boolean] = {
      val tail = Integer.toBinaryString(int).map {
        case '1' => true
        case _ => false
      }.toList
      val head = List.fill(length - tail.length)(false)
      (head ::: tail).toVector
    }

  }

}