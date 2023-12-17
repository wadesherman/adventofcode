package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import util.Utils.{rotateLeft, rotateRight}

class UtilsSpec extends AnyFlatSpec {
  it should "rotate a matrix" in {

    val matrix = Array(
      Array(1,2,3),
      Array(4,5,6),
      Array(7,8,9),
    )

    val rotated = Array(
      Array(7,4,1),
      Array(8,5,2),
      Array(9,6,3),
    )

    rotateRight(matrix) shouldBe rotated

    rotateLeft(rotateRight(matrix)) shouldBe matrix
  }
}
