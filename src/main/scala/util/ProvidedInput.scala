package util

trait ProvidedInput {

  val year: Int
  val day: Int
  val testInput: Array[String]
  def providedInput: Array[String] = Utils.readInput(s"y${year.toString}/day${day.toString}")

  case class TimedFunction[O](result: O, time: Long) {
    override def toString: String = s"${result} ${time}ms"
  }
  def run[O](f: => O): TimedFunction[O] = {
    val t1 = System.currentTimeMillis()
    val result = f
    val t2 = System.currentTimeMillis()
    TimedFunction(result, (t2 - t1))
  }
}
