package com.earnest.battleship

import Ship.Attackable

object Ship {
  type Attackable = Option[Set[Coordinates]]

  implicit class ShipHelper(ship: Ship) {
    def copy(attackable: Attackable): Ship = { ship match {
        case Destroyer(name, abbrv, length, _) => Destroyer(name, abbrv, length, attackable)
        case Submarine(name, abbrv, length, _) => Submarine(name, abbrv, length, attackable)
        case Cruiser(name, abbrv, length, _) => Cruiser(name, abbrv, length, attackable)
        case Battleship(name, abbrv, length, _) => Battleship(name, abbrv, length, attackable)
        case Carrier(name, abbrv, length, _) => Carrier(name, abbrv, length, attackable)
      }
    }
  }
}

sealed abstract class Ship {
  def name: String
  def abbrv: Char
  def length: Int
  def attackable: Attackable
}

case class Destroyer(name: String = "Destroyer", abbrv: Char = 'D', length: Int = 2, attackable: Attackable = None) extends Ship
case class Submarine(name: String = "Submarine", abbrv: Char = 'S', length: Int = 3, attackable: Attackable = None) extends Ship
case class Cruiser(name: String = "Cruiser", abbrv: Char = 'c', length: Int = 3, attackable: Attackable = None) extends Ship
case class Battleship(name: String = "Battleship", abbrv: Char = 'B', length: Int = 4, attackable: Attackable = None) extends Ship
case class Carrier(name: String = "Carrier", abbrv: Char = 'C', length: Int = 5, attackable: Attackable = None) extends Ship
