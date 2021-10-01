module Tron exposing (main)

import Playground exposing (..)
import List.Nonempty as Nonempty exposing (Nonempty)
import Set

type alias Point = (Int, Int)

type Direction = Up | Down | Left | Right

type alias Player =
  { direction : Direction
  , points : Nonempty Point
  }

type Result
  = Win1
  | Win2
  | Tie

type Memory
  = Result Result
  | Game
    { player1 : Player
    , player2 : Player
    , time : Int
    }


main = game draw update init

init : Memory
init = Game
  { player1 =
      { direction = Right
      , points = Nonempty.singleton (5, 24)
      }
  , player2 =
      { direction = Left
      , points = Nonempty.singleton (24, 5)
      }
  , time = 0
  }

draw : Computer -> Memory -> List Shape
draw {screen} memory =
  let size = min screen.width screen.height in
  case memory of
    Result result -> drawWin size result
    Game {player1, player2} -> drawGame size player1 player2

drawWin : Float -> Result -> List Shape
drawWin size result =
  let
    (color, text) =
      case result of
        Win1 -> (blue, "Player 1 wins!")
        Win2 -> (red, "Player 2 wins!")
        Tie -> (black, "It's a tie.")
  in
  [ square color size
  , words white text
      |> scale 5
      |> moveUp 50
  , words white "Press space to restart."
      |> scale 2
      |> moveDown 50
  ]

drawGame : Float -> Player -> Player -> List Shape
drawGame size player1 player2 =
  let
    toSquare color (x, y) =
      square color (size / 30 + 1)
        |> moveX (size / 30 * (toFloat x - 15) + size / 60)
        |> moveY (size / 30 * (toFloat y - 15) + size / 60)
  in
  [ square gray size ] ++
  List.map (toSquare blue) (Nonempty.toList player1.points) ++
  List.map (toSquare red) (Nonempty.toList player2.points)

update : Computer -> Memory -> Memory
update {keyboard} memory =
  case memory of
    Result result ->
      if keyboard.space then init
      else Result result
    Game {player1, player2, time} ->
      updateGame keyboard player1 player2 time

updateGame : Keyboard -> Player -> Player -> Int -> Memory
updateGame keyboard player1 player2 time =
  let
    keyDown k = Set.member k (keyboard.keys)

    direction1 =
      if keyDown "w" && player1.direction /= Down then Up
      else if keyDown "s" && player1.direction /= Up then Down
      else if keyDown "a" && player1.direction /= Right then Left
      else if keyDown "d" && player1.direction /= Left then Right
      else player1.direction

    direction2 =
      if keyboard.up then Up
      else if keyboard.down then Down
      else if keyboard.left then Left
      else if keyboard.right then Right
      else player2.direction

    point1 = move direction1 (Nonempty.head player1.points)
    point2 = move direction2 (Nonempty.head player2.points)

    points1 =
      if modBy 6 time == 0
        then Nonempty.cons point1 player1.points
      else player1.points

    points2 =
      if modBy 6 time == 0
        then Nonempty.cons point2 player2.points
      else player2.points

    newPlayer1 =
      { player1
      | direction = direction1
      , points = points1
      }

    newPlayer2 =
      { player2
      | direction = direction2
      , points = points2
      }

    dead1 =
      Nonempty.member point1 player1.points ||
      Nonempty.member point1 points2

    dead2 =
      Nonempty.member point2 points1 ||
      Nonempty.member point2 player2.points
  in
  case (dead1, dead2) of
    (True, True) -> Result Tie
    (False, True) -> Result Win1
    (True, False) -> Result Win2
    (False, False) -> Game
      { player1 = newPlayer1
      , player2 = newPlayer2
      , time = time + 1
      }

move : Direction -> Point -> Point
move direction (x, y) =
  case direction of
    Up -> (x, modBy 30 (y + 1))
    Down -> (x, modBy 30 (y - 1))
    Left -> (modBy 30 (x - 1), y)
    Right -> (modBy 30 (x + 1), y)
