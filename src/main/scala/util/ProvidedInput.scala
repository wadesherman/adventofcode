package util

trait ProvidedInput {

  val year: Int
  val day: Int
  val testInput: Array[String]
  def providedInput: Array[String] =  Utils.readInput(s"y${year.toString}/day${day.toString}")

}
