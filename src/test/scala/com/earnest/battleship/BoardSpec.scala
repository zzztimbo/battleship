package com.earnest.battleship

import com.earnest.battleship.Board.ShipHelper

class BoardSpec extends EarnestSpec {
  "A board of size 5" should "contain 25 coordinates in its mapState" in {
    Board(5).mapState.size shouldEqual 25
  }

  "A board of size 5" should "have 5 new lines when converted to a sequence of strings" in {
    Board(5).toStringSeq().filter(_.contains("\n")).size shouldEqual 5
  }

  "Adding a ship to the board" should "yield a new board with that ship" in {
    val board = Board(5)
    val d = Destroyer()
    val coordinates = Coordinates(1, 1)
    val newBoard = board.putShip(d, coordinates, ShipOrientation.Horizontal)
    val attackableShipCoords = d.generateCoordinates(board, coordinates, ShipOrientation.Horizontal)

    newBoard.attackableShips.contains(d.abbrv) shouldEqual true
    newBoard.attackableShips.get(d.abbrv).get.attackable shouldBe Some(attackableShipCoords.toSet)
  }

  "Adding the same ship twice" should "throw an exception" in {
    val board = Board(5)
    val d = Destroyer()
    val coordinates = Coordinates(1, 1)
    val board1 = board.putShip(d, coordinates, ShipOrientation.Horizontal)

    intercept[AlreadyOccupiedException] {
      board1.putShip(d, coordinates, ShipOrientation.Horizontal)
    }
  }

  "Adding an overlapping ship" should "throw an exception" in {
    val board = Board(5)
    val d = Destroyer()
    val s = Submarine()

    val coordinates = Coordinates(1, 1)
    val board1 = board.putShip(d, coordinates, ShipOrientation.Horizontal)

    intercept[AlreadyOccupiedException] {
      board1.putShip(s, coordinates, ShipOrientation.Horizontal)
    }
  }

  "Placing two ships on a board" should "yield a valid pretty printed map" in {
    val board = Board(5)
    val d = Destroyer()
    val s = Submarine()

    val coordinates = Coordinates(1, 1)
    val coordinates2 = Coordinates(1, 2)
    val board1 = board.putShip(d, coordinates, ShipOrientation.Horizontal)
    val board2 = board1.putShip(s, coordinates2, ShipOrientation.Horizontal)

    board2.prettyPrint()
  }

  "Attacking and sinking Destroyer" should "yield the correct outcome" in {
    val board = Board(5)
    val d = Destroyer()
    val coordinates = Coordinates(1, 1)
    val board1 = board.putShip(d, coordinates, ShipOrientation.Horizontal)
    val boardOutcome1 = board1.attack(Coordinates(1, 1))
    val boardOutcome2 = boardOutcome1.board.attack(Coordinates(2, 1))

    boardOutcome1.outcome shouldBe Outcomes.Hit
    boardOutcome2.outcome shouldBe Outcomes.Win

    boardOutcome1.board.prettyPrint()
    println()
    boardOutcome2.board.prettyPrint()
  }

  "Attacking and sinking Destroyer while Submarine is still alive" should "yield the correct outcome" in {
    val board = Board(5)
    val d = Destroyer()
    val s = Submarine()

    val coordinates = Coordinates(1, 1)
    val coordinates2 = Coordinates(1, 2)
    val board1 = board.putShip(d, coordinates, ShipOrientation.Horizontal)
    val board2 = board1.putShip(s, coordinates2, ShipOrientation.Horizontal)

    val boardOutcome1 = board2.attack(Coordinates(1, 1))
    val boardOutcome2 = boardOutcome1.board.attack(Coordinates(2, 1))

    boardOutcome1.outcome shouldBe Outcomes.Hit
    boardOutcome2.outcome shouldBe Outcomes.Sunk

    boardOutcome1.board.prettyPrint()
    println()
    boardOutcome2.board.prettyPrint()
  }

  "Attacking a Destroyer and missing" should "yield the correct outcome" in {
    val board = Board(5)
    val d = Destroyer()
    val coordinates = Coordinates(1, 1)
    val board1 = board.putShip(d, coordinates, ShipOrientation.Horizontal)
    val boardOutcome1 = board1.attack(Coordinates(4, 1))

    boardOutcome1.outcome shouldBe Outcomes.Miss
    boardOutcome1.board.prettyPrint()
  }

  "Attacking the same coordiantes twice" should "yield the correct outcome" in {
    val board = Board(5)
    val d = Destroyer()
    val coordinates = Coordinates(1, 1)
    val board1 = board.putShip(d, coordinates, ShipOrientation.Horizontal)
    val boardOutcome1 = board1.attack(Coordinates(4, 1))
    val boardOutcome2 = boardOutcome1.board.attack(Coordinates(4, 1))

    boardOutcome1.outcome shouldBe Outcomes.Miss
    boardOutcome2.outcome shouldBe Outcomes.AlreadyTaken
    boardOutcome2.board shouldBe boardOutcome1.board

    boardOutcome2.board.prettyPrint()
  }
}
