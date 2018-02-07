package com.earnest.battleship

import Board.ShipHelper

class ShipsSpec extends EarnestSpec {
  "Placing a Destroyer" should "yield the correct list of coordinates" in {
    val d = Destroyer()
    val board = Board(5)

    val c = Coordinates(1, 1)
    val coordsSet = d.generateCoordinates(board, c, ShipOrientation.Horizontal).toSet

    coordsSet.size shouldBe 2
    coordsSet.contains(Coordinates(1,1)) shouldBe true
    coordsSet.contains(Coordinates(2,1)) shouldBe true

    val c2 = Coordinates(1, 1)
    val coordsSet2 = d.generateCoordinates(board, c2, ShipOrientation.Vertical).toSet

    coordsSet2.size shouldBe 2
    coordsSet2.contains(Coordinates(1,2)) shouldBe true
    coordsSet2.contains(Coordinates(1,1)) shouldBe true
  }

  "Placing a Destroyer incorrectly" should "yield a PlacementException" in {
    val d = Destroyer()
    val board = Board(5)

    val c = Coordinates(5, 5)

    intercept[PlacementException] {
      d.generateCoordinates(board, c, ShipOrientation.Horizontal)
    }

    val c2 = Coordinates(5, 5)

    intercept[PlacementException] {
      d.generateCoordinates(board, c2, ShipOrientation.Vertical)
    }

    val c3 = Coordinates(7, 1)

    intercept[PlacementException] {
      d.generateCoordinates(board, c3, ShipOrientation.Vertical)
    }

    val c4 = Coordinates(1, 7)

    intercept[PlacementException] {
      d.generateCoordinates(board, c4, ShipOrientation.Horizontal)
    }
  }
}
