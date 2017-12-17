module Main exposing (..)

import Collage exposing (Form, collage, filled, group, move, outlined, rect, solid, square)
import Color exposing (black)
import Dict
import Element
import Html exposing (Html, text)
import Keyboard
import Piece exposing (Orientation(..), Piece(..), Shape(..))
import Random
import Random.Extra
import Set exposing (Set)
import Text
import Time


numCols =
    10


numRows =
    20


fromJust : Maybe a -> a
fromJust maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.crash "fromJust given Nothing"


pieceGenerator : Random.Generator Piece
pieceGenerator =
    Random.map (flip Piece North << fromJust) <| Random.Extra.sample [ OShape ]



--Random.map (flip Piece North << fromJust) <| Random.Extra.sample [ IShape, JShape, LShape, OShape, SShape, TShape, ZShape ]
-- XXX current speed


type alias State =
    { currentScore : Int
    , currentPiece : Piece
    , currentPiecePosition : ( Int, Int )
    , nextPiece : Piece
    , fixatedBlocks : Set ( Int, Int )
    , dropping : Bool
    }


type Model
    = Uninitialized
    | Initialized State
    | GameOver Int


type Msg
    = Initialize Piece Piece
    | NextPiece Piece
    | Tick
    | MoveLeft
    | MoveRight
    | Drop
    | StopDrop
    | Rotate
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( Uninitialized, Random.generate (uncurry Initialize) <| Random.pair pieceGenerator pieceGenerator )


anyFixated : State -> Int -> Bool
anyFixated state offset =
    let
        blocks =
            Piece.getBlocks state.currentPiece

        ( cx, cy ) =
            state.currentPiecePosition

        newBlocks =
            List.map (\( x, y ) -> ( x + offset + cx, y + cy )) blocks

        blockIsFixated pos =
            Set.member pos state.fixatedBlocks
    in
    List.any blockIsFixated newBlocks


moveCurrentPieceLeft : State -> State
moveCurrentPieceLeft model =
    let
        ( x, y ) =
            model.currentPiecePosition
    in
    if x == 0 then
        model
    else if anyFixated model -1 then
        model
    else
        { model | currentPiecePosition = ( x - 1, y ) }


moveCurrentPieceRight : State -> State
moveCurrentPieceRight model =
    let
        ( x, y ) =
            model.currentPiecePosition

        width =
            Piece.getWidth model.currentPiece
    in
    if x + width == numCols then
        model
    else if anyFixated model 1 then
        model
    else
        { model | currentPiecePosition = ( x + 1, y ) }


rotateCurrentPiece : State -> State
rotateCurrentPiece model =
    let
        ( x, y ) =
            model.currentPiecePosition

        newPiece =
            Piece.rotate model.currentPiece

        newWidth =
            Piece.getWidth newPiece
    in
    if x + newWidth > numCols then
        { model | currentPiece = newPiece, currentPiecePosition = ( numCols - newWidth, y ) }
    else
        { model | currentPiece = newPiece }


movePieceDown : State -> State
movePieceDown state =
    let
        ( x, y ) =
            state.currentPiecePosition
    in
    { state | currentPiecePosition = ( x, y + 1 ) }


translateRelativeTo : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
translateRelativeTo ( dx, dy ) ( x, y ) =
    ( dx + x, dy + y )


detectCollisions : State -> Bool
detectCollisions state =
    let
        pieceBlocks =
            List.map (translateRelativeTo state.currentPiecePosition) <| Piece.getBlocks state.currentPiece
    in
    List.any (\( _, y ) -> y >= numRows) pieceBlocks || List.any (\point -> Set.member point state.fixatedBlocks) pieceBlocks


fixate : State -> State
fixate state =
    let
        pieceBlocks =
            List.map (translateRelativeTo state.currentPiecePosition) <| Piece.getBlocks state.currentPiece
    in
    { state | fixatedBlocks = Set.union state.fixatedBlocks <| Set.fromList pieceBlocks }


countBlocksByRow : List ( Int, Int ) -> List ( Int, Int )
countBlocksByRow blocks =
    let
        incrementCount ( _, row ) countDict =
            Dict.update row (Just << (+) 1 << Maybe.withDefault 0) countDict
    in
    Dict.toList <| List.foldl incrementCount Dict.empty blocks


checkForCompleteRows : State -> State
checkForCompleteRows state =
    let
        blockCounts =
            countBlocksByRow <| Set.toList state.fixatedBlocks

        completeRows =
            Set.fromList <|
                List.filterMap
                    (\( row, count ) ->
                        if count == numCols then
                            Just row
                        else
                            Nothing
                    )
                    blockCounts

        maxCompletedRow =
            List.maximum <| Set.toList completeRows
    in
    case maxCompletedRow of
        Nothing ->
            state

        Just maxCompletedRow ->
            let
                completedRowsRemoved =
                    List.filter (\( _, row ) -> not <| Set.member row completeRows) <| Set.toList state.fixatedBlocks

                shiftDown ( x, row ) =
                    if row < maxCompletedRow then
                        ( x, row + Set.size completeRows )
                    else
                        ( x, row )

                shiftedRows =
                    List.map shiftDown completedRowsRemoved
            in
            { state | fixatedBlocks = Set.fromList <| shiftedRows, currentScore = state.currentScore + 100 * Set.size completeRows }



-- XXX hardcoded value


advance : State -> State
advance state =
    { state | currentPiece = state.nextPiece, currentPiecePosition = ( 0, 0 ) }


checkGameOver : State -> Model
checkGameOver state =
    if detectCollisions state then
        GameOver state.currentScore
    else
        Initialized state



-- LTA: why *can't* I have this as a new message?


fixateAndAdvance : State -> Model
fixateAndAdvance state =
    checkGameOver <| advance <| checkForCompleteRows <| fixate <| state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameOver _ ->
            ( model, Cmd.none )

        Uninitialized ->
            case msg of
                Initialize currentPiece nextPiece ->
                    let
                        model =
                            Initialized
                                { currentScore = 0
                                , currentPiece = currentPiece
                                , currentPiecePosition = ( 0, 0 )
                                , nextPiece = nextPiece
                                , fixatedBlocks = Set.empty
                                , dropping = False
                                }
                    in
                    ( model, Cmd.none )

                -- LTA
                _ ->
                    Debug.crash <| "Somehow you managed to get a " ++ toString msg ++ " msg in an uninitialized state o_O"

        Initialized state ->
            case msg of
                -- LTA
                Initialize _ _ ->
                    Debug.crash "Somehow you managed to get an initialize msg in an initialized state o_O"

                NoOp ->
                    ( model, Cmd.none )

                MoveLeft ->
                    ( Initialized <| moveCurrentPieceLeft state, Cmd.none )

                MoveRight ->
                    ( Initialized <| moveCurrentPieceRight state, Cmd.none )

                Rotate ->
                    ( Initialized <| rotateCurrentPiece state, Cmd.none )

                Tick ->
                    let
                        newState =
                            movePieceDown state
                    in
                    if detectCollisions newState then
                        ( fixateAndAdvance state, Random.generate NextPiece pieceGenerator )
                    else
                        ( Initialized newState, Cmd.none )

                Drop ->
                    ( Initialized { state | dropping = True }, Cmd.none )

                StopDrop ->
                    ( Initialized { state | dropping = False }, Cmd.none )

                NextPiece piece ->
                    ( Initialized { state | nextPiece = piece }, Cmd.none )


translateKeyDown : Keyboard.KeyCode -> Msg
translateKeyDown keycode =
    case keycode of
        38 ->
            Rotate

        40 ->
            Drop

        37 ->
            MoveLeft

        39 ->
            MoveRight

        _ ->
            NoOp


translateKeyUp : Keyboard.KeyCode -> Msg
translateKeyUp keycode =
    if keycode == 40 then
        StopDrop
    else
        NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Uninitialized ->
            Sub.none

        GameOver _ ->
            Sub.none

        Initialized state ->
            let
                tickInterval =
                    if state.dropping then
                        Time.second / 20
                    else
                        Time.second
            in
            Sub.batch
                [ Keyboard.downs translateKeyDown
                , Keyboard.ups translateKeyUp
                , Time.every tickInterval <| always Tick
                ]


boardWidth =
    Piece.blockSize * numCols


boardHeight =
    Piece.blockSize * numRows


halfBoardWidth =
    boardWidth // 2


halfBoardHeight =
    boardHeight // 2


totalWidth =
    600


totalHeight =
    600


halfTotalWidth =
    totalWidth // 2


halfTotalHeight =
    totalHeight // 2


boardOffsetX =
    20


boardOffsetY =
    20



-- one of my best struggles with Collage is that (0, 0) isn't the TL corner


renderBoard : Piece -> ( Int, Int ) -> List ( Int, Int ) -> Form
renderBoard currentPiece ( pieceX, pieceY ) fixatedBlocks =
    let
        renderedPiece =
            Piece.render currentPiece

        border =
            outlined (solid black) <| rect boardWidth boardHeight

        boardX =
            toFloat -(halfTotalWidth - halfBoardWidth - boardOffsetX)

        boardY =
            toFloat <| halfTotalHeight - halfBoardHeight - boardOffsetY

        positionedPiece =
            move ( toFloat (-halfBoardWidth + boardOffsetX // 2 + pieceX * Piece.blockSize), toFloat (halfBoardHeight - boardOffsetY // 2 - pieceY * Piece.blockSize) ) renderedPiece

        renderedBlocks =
            move ( toFloat (-halfBoardWidth + boardOffsetX // 2), toFloat (halfBoardHeight - boardOffsetY // 2) ) <| group <| List.map (\( x, y ) -> move ( toFloat x * Piece.blockSize, toFloat -y * Piece.blockSize ) <| filled black <| square Piece.blockSize) fixatedBlocks
    in
    move ( boardX, boardY ) <| group [ positionedPiece, border, renderedBlocks ]



-- XXX center within the bordered area


renderNext : Piece -> Form
renderNext nextPiece =
    let
        border =
            outlined (solid black) <| rect 100 100

        renderedPiece =
            Piece.render nextPiece
    in
    move
        ( toFloat <| boardOffsetX + boardWidth + boardOffsetX - halfTotalWidth + 50
        , toFloat <| halfTotalHeight - boardOffsetY - 50
        )
    <|
        group [ border, renderedPiece ]


renderScore : Int -> Form
renderScore score =
    let
        renderedScore =
            Collage.text <| Text.fromString <| toString score
    in
    move
        ( toFloat <| boardOffsetX + boardWidth + boardOffsetX - halfTotalWidth + 50
        , toFloat <| halfTotalHeight - boardOffsetY - 50
        )
        renderedScore



-- XXX hardcoded value


view : Model -> Html Msg
view model =
    case model of
        GameOver score ->
            text <| "Game over! Your score was " ++ toString score

        Uninitialized ->
            text ""

        Initialized state ->
            Element.toHtml <|
                collage totalWidth
                    totalHeight
                    [ renderBoard state.currentPiece state.currentPiecePosition <| Set.toList state.fixatedBlocks
                    , renderNext state.nextPiece
                    , renderScore state.currentScore
                    ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
