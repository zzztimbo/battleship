package com.earnest.battleship

import com.earnest.battleship.Board.{AttackableShips, CoordState, EmptyBoardMap, MapState, ShipHelper}

import scala.util.Either

class Board(val size: Int, val initMapState: Option[MapState] = None, val initAttackableShips: Option[AttackableShips] = None) {

  val attackableShips: AttackableShips = {
    initAttackableShips match {
      case None => Map.empty[Char, Ship]
      case _ => initAttackableShips.get
    }
  }

  val mapState: MapState = initMapState match {
    case None => EmptyBoardMap(size)
    case _ => initMapState.get
  }

  def get(coords: Coordinates): CoordState = mapState(coords)

  def putShip(ship: Ship, coord: Coordinates, orientation: ShipOrientation.Value): Board = {
    val tentativeCoords = ship.generateCoordinates(this, coord, orientation)

    val unusedMapCoords = tentativeCoords
      .map(coords => mapState(coords))
      .filter(_.isLeft)
      .filter(_ == Left(false))

    if (unusedMapCoords.size != tentativeCoords.size)
      throw new AlreadyOccupiedException("placement coordinates either already contain a ship or have already been fired upon")

    val newShip = ship match {
      case Destroyer(name, abbrv, length, _) => Destroyer(name, abbrv, length, attackable = Some(tentativeCoords.toSet))
      case Submarine(name, abbrv, length, _) => Submarine(name, abbrv, length, attackable = Some(tentativeCoords.toSet))
      case Cruiser(name, abbrv, length, _) => Cruiser(name, abbrv, length, attackable = Some(tentativeCoords.toSet))
      case Battleship(name, abbrv, length, _) => Battleship(name, abbrv, length, attackable = Some(tentativeCoords.toSet))
      case Carrier(name, abbrv, length, _) => Carrier(name, abbrv, length, attackable = Some(tentativeCoords.toSet))
    }

    val newMapState = mapState ++ tentativeCoords.map(coord => coord -> Right(newShip.abbrv)).toMap
    val newAttackableShips = attackableShips + (ship.abbrv -> newShip)
    Board(size, initMapState = newMapState, initAttackableShips = newAttackableShips)
  }

  def attack(coords: Coordinates): BoardOutcome = {
    val coordState = mapState(coords)

    coordState match {
      case Left(true) => BoardOutcome(this, Outcomes.AlreadyTaken)

      case Left(false) => {
        val mapStateEntry = Map(coords -> Left(true))
        val mergedMap = mapState ++ mapStateEntry
        val newBoard = Board(size, mergedMap, attackableShips)
        BoardOutcome(newBoard, Outcomes.Miss)
      }

      case Right(shipAbbrv) => (attackableShips(shipAbbrv).attackable.get - coords).size  match {
        case 0 => {
          val mapStateEntry = Map(coords -> Left(true))
          val mergedMap = mapState ++ mapStateEntry
          val newAttackableShips = attackableShips - shipAbbrv
          val newBoard = Board(size, mergedMap, newAttackableShips)
          newAttackableShips.size match {
            case 0 => BoardOutcome(newBoard, Outcomes.Win)
            case _ => BoardOutcome(newBoard, Outcomes.Sunk)
          }
        }
        case _ => {
          val ship = attackableShips(shipAbbrv)
          val newAttackable = attackableShips(shipAbbrv).attackable.get - coords
          val newShip = ship.copy(attackable = Some(newAttackable))
          val mapStateEntry = Map(coords -> Left(true))
          val mergedMap = mapState ++ mapStateEntry
          val newAttackableShips = attackableShips ++ Map(shipAbbrv -> newShip)
          val newBoard = Board(size, mergedMap, newAttackableShips)
          BoardOutcome(newBoard, Outcomes.Hit)
        }
      }
    }
  }
}

object Board {
  type HasAttacked = Boolean
  type ShipAbbrv = Char
  type CoordState = Either[HasAttacked, ShipAbbrv]
  type MapState = Map[Coordinates, CoordState]
  type AttackableShips = Map[Char, Ship]

  private def EmptyBoardSeq(size: Int): Seq[(Coordinates, CoordState)] =
    for {
      i <- 1 to size
      j <- 1 to size
    } yield Coordinates(j, i) -> Left(false)

  def EmptyBoardMap(size: Int): MapState = EmptyBoardSeq(size).toMap

  def apply(): Board =  new Board(size = 10)
  def apply(size: Int): Board =  new Board(size)
  def apply(size: Int, initMapState: MapState, initAttackableShips: AttackableShips) = new Board(size, Some(initMapState), Some(initAttackableShips))

  implicit class CoordStateHelper(coordState: CoordState) {
    def toChar(): Char = {
      coordState match {
        case Left(hasAttacked) => hasAttacked match {
          case true => 'x'
          case _    => 'o'
        }
        case Right(shipAbbrv) => shipAbbrv
      }
    }
  }

  implicit class BoardHelper(board: Board) {
    def toStringSeq(): Seq[String] = {
      val coordinates = for {
        i <- 1 to board.size
        j <- 1 to board.size
      } yield Coordinates(j, i)

      coordinates.map(coords => {
        val char = board.get(coords).toChar()
        coords.x match {
          case board.size => {
            s"   ${char}   \n"
          }
          case _ => {
            s"   $char   "
          }
        }
      })
    }
    def prettyPrint(): Unit = {
      board.toStringSeq().foreach(print)
    }
  }

  implicit class ShipHelper(ship: Ship) {
    def generateCoordinates(board: Board, coordinates: Coordinates, orientation: ShipOrientation.Value): Seq[Coordinates] = {
      orientation match {
        case ShipOrientation.Horizontal => {
          val coords = for (x <- coordinates.x until coordinates.x + ship.length) yield (Coordinates(x, coordinates.y))
          if (coordinates.x + ship.length > board.size || coordinates.y > board.size)
            throw new PlacementException(s"all placement coordinates do not fit on board: $coords")
          coords
        }
        case ShipOrientation.Vertical => {
          val coords = for (y <- coordinates.y until coordinates.y + ship.length) yield (Coordinates(coordinates.x, y))
          if (coordinates.y + ship.length > board.size || coordinates.x > board.size)
            throw new PlacementException(s"all placement coordinates do not fit on board: $coords")
          coords
        }
      }
    }
  }
}

case class Coordinates(x: Int, y: Int)
case class PlacementException(private val message: String = "", private val cause: Throwable = None.orNull) extends Exception(message, cause)
case class AlreadyOccupiedException(private val message: String = "", private val cause: Throwable = None.orNull) extends Exception(message, cause)
case class BoardOutcome(board: Board, outcome: Outcomes.Value)

object ShipOrientation extends Enumeration {
  val Vertical, Horizontal = Value
}