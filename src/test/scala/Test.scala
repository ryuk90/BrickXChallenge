import java.util

import org.scalatest._


class RoverMovesTest extends FlatSpec with Matchers {

  "A rover" should "move through the plateau" in {
    val plateau = (10, 10)
    val rover = new Rover
    rover.x = 0
    rover.y = 0
    rover.d = "E"

    val moves = "MLMRM"
    val expected = "2 1 E"
    val move_list = moves.split("")

    move_list.map {
      move =>
        rover.move(move, plateau)
    }

    rover.toString() should be (expected)
  }

  it should "throw an Exception when go out the plateau" in {
    val plateau = (4, 4)
    val rover = new Rover
    rover.x = 0
    rover.y = 0
    rover.d = "W"

    val moves = "M"
    val move_list = moves.split("")

    a [Exception] should be thrownBy {
      move_list.map {
        move =>
          rover.move(move, plateau)
      }
    }
  }

}