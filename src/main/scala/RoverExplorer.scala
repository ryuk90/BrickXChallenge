
import scala.collection.mutable.ArrayBuffer

/**
 * Created by Mirko Saiu on 18/02/16.
 *
 *
 *
 * INSTRUCTIONS: Run this file
 *
 *
 *
 */

object RoverExplorer {

  var plateau: (Int,Int) = _
  var rovers: ArrayBuffer[Rover] = ArrayBuffer()
  var rovers_moves: ArrayBuffer[String] = ArrayBuffer()
  var cell_count: Int = _
  var num_rovers: Int = 0



  def main (args: Array[String]) : Unit = {

    setup_plateau

    // FLAG
    var is_number_inserted = false
    // ASKS THE QUANTITY OF ROVER TO CREATE
    while(!is_number_inserted) {
      try{
        num_rovers = readLine("Please insert how many rovers you want on the plateau: ").toInt
        if(num_rovers > 0) is_number_inserted = true
        else println("Insert at least one")
      }
      catch {
        case e: Exception =>
      }
    }


    setup_rovers

    move_rovers

  }


  def setup_rovers = {
    // ASKING POSITIONS AND MOVES FOR THE ROVERS
    while (rovers.length < num_rovers){
      val input = readLine("Please insert rover coordinates and direction: ")
      var error = false

      try {
        val splitted = input.split(" ")
        val x = splitted.apply(0).toInt
        val y = splitted.apply(1).toInt
        val d = splitted.apply(2)


        // CHECKS IF THE ROVER WOULD BE OUT OF THE PLATEAU
        if(x>plateau._1 || x<0) {
          error = true
          println("The rover cannot stay out of the plateau")
        }
        if(y>plateau._2 || y<0) {
          error = true
          println("The rover cannot stay out of the plateau")
        }


        // CHECKS THE VALUE OF THE DIRECTION
        d match {
          case "N" | "S" | "W" | "E" =>
            if (!error) {
              val rover = new Rover
              rover.x = x
              rover.y = y
              rover.d = d
              rovers.+=(rover)
            }
          case _ =>
            error = true
            println("The direction must be one of these (case sensitive): N S W E")
        }
      } catch {
        case e: Exception =>
          error = true
          println("Use the format: x y d")
          println("Example: 1 2 N")
          println("Try again!")
      }

      // IF NO ERRORS, ASKS FOR THE MOVES
      if(error == false) {
        val input = readLine("Please insert rover moves: ")

        if(input.split("").filter(x => x!="M" && x!="L" && x!="R" && x!="").length > 0){
          println("The moves must be one of these (case sensitive): M L R")
        } else {
          rovers_moves.+=(input)
        }
      }
    }
  }








  def setup_plateau = {

    // FLAGS
    var are_coords_inserted = false


    // ASKING COORDINATES
    while (!are_coords_inserted) {
      val input = readLine("Please insert upper-right coordinates: ")
      try {
        val splitted = input.split(" ")
        val x = splitted.apply(0).toInt
        val y = splitted.apply(1).toInt
        plateau = (x, y)

        // CHECKS IF THE INSERTED COORDINATES MAKE A RECTANGULAR PLATEAU
        if(x > 0 || y > 0)
          are_coords_inserted = true
        else
          println("x or y must be greater than 0")

      } catch {
        case e: Exception =>
          println("Use the format: x y")
          println("Example: 1 2")
          println("Try again!")
      }
    }


    // NUMBER OF CELLS OF THE PLATEAU
    cell_count = (plateau._1+1) * (plateau._2+1)

  }






  def move_rovers = {

    rovers.zipWithIndex.map{
      case (rover,i) =>
        val move_list = rovers_moves.apply(i).split("")

        move_list.map{
          move =>
            // DO A SINGLE MOVEMENT
            rover.move(move, plateau)
            // CHECKS IF
            if(rover.is_not_inside_plateau(plateau)) {
              println("The rover quits the plateau")
            }
        }

        println(rover)
    }


  }




}
