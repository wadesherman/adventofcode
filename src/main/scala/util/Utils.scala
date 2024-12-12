package util

import scala.collection.mutable
import scala.io.Source
import scala.reflect.ClassTag

object Utils {
  def readInput(s: String): Array[String] = Source.fromResource(s).getLines().toArray

  def lazyList: LazyList[BigInt] = LazyList.iterate(BigInt(0))(_ + 1)

  /**
   *  Example Usage:
   *  lazy val toUpper: String => String = memoize {
   *    case str => s.toUpperCase
   *  }
   */
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = {
      getOrElseUpdate(key, f(key))
    }
  }

  def rotateLeft[T](input: Array[Array[T]])(implicit tag: ClassTag[T]): Array[Array[T]] = {
    val w = input.head.length - 1
    val h = input.length - 1
    ((0 to w).reverse)
      .map(i =>
        (0 to h)
          .map(j => input(j)(i)).toArray
      )
      .toArray
  }

  def rotateRight[T](input: Array[Array[T]])(implicit tag: ClassTag[T]): Array[Array[T]] = {
    val w = input.head.length - 1
    val h = input.length - 1
    ((0 to w))
      .map(i =>
        (0 to h).reverse
          .map(j => input(j)(i)).toArray
      )
      .toArray
  }

  def hexToBigInt(hex: String): BigInt =
    hex.toList.map("0123456789abcdef".indexOf(_)).foldLeft(BigInt(0)){ case (z, h) => z * 16 + h}
}
