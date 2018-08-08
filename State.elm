module State exposing (..)

import Piece exposing (Piece)
import Set exposing (Set)


numCols : Int
numCols =
    10


numRows : Int
numRows =
    20


blockSize : Int
blockSize =
    30


offsetX : Int
offsetX =
    blockSize


offsetY : Int
offsetY =
    blockSize


type alias State =
    { currentScore : Int
    , currentPiece : Piece
    , currentPiecePosition : ( Int, Int )
    , nextPiece : Piece
    , fixatedBlocks : Set ( Int, Int, String )
    , dropping : Bool
    }


type alias Block =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , color : String
    }
