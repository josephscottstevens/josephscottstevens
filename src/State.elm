module State exposing (Block, blockSize, numCols, numRows, offsetX, offsetY)


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


type alias Block =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , color : String
    }
