
/**
 * Created by ryuk on 18/02/16.
 */
class Rover {

  var x: Int = 0
  var y: Int = 0
  var d: String = ""


  def move(m: String, plateau: (Int,Int)) = {
    m match {
      // MOVE ONE STEP AHEAD
      case "M" =>

        d match {
          case "N" => y += 1
          case "S" => y -= 1
          case "E" => x += 1
          case "W" => x -= 1
        }

      // SPIN LEFT
      case "L" =>

        d match {
          case "N" => d = "W"
          case "S" => d = "E"
          case "E" => d = "N"
          case "W" => d = "S"
        }

      // SPIN RIGHT
      case "R" =>

        d match {
          case "N" => d = "E"
          case "S" => d = "W"
          case "E" => d = "S"
          case "W" => d = "N"
        }

      case _ =>
    }

    if(is_not_inside_plateau(plateau)) throw new Exception
  }

  def is_not_inside_plateau (plateau_coords: (Int,Int)): Boolean = {
    x<0 || y<0 || x>plateau_coords._1 || y>plateau_coords._2
  }

  override def toString(): String = {
    x + " " + y + " " + d
  }
}
