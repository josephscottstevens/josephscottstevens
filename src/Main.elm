module Main exposing (main)

import Browser
import Browser.Events
import Dict
import Draw exposing (px)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events as Events
import Json.Decode as Json
import Keyboard
import Piece exposing (Piece)
import Random
import Set exposing (Set)
import State exposing (blockSize, numCols, numRows)
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { currentScore : Int
    , currentPiece : Piece
    , currentPiecePosition : ( Int, Int )
    , nextPiece : Piece
    , fixatedBlocks : Set ( Int, Int, String )
    , dropping : Bool
    , state : State
    }


type State
    = Uninitialized
    | Initialized
    | GameOver Int
    | Error String


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { currentScore = 0
      , currentPiece = Piece.startingPiece
      , currentPiecePosition = ( 0, 0 )
      , nextPiece = Piece.startingPiece
      , fixatedBlocks = Set.empty
      , dropping = False
      , state = Uninitialized
      }
    , Random.generate (uncurry Initialize) <| Random.pair Piece.pieceGenerator Piece.pieceGenerator
    )


type Msg
    = Tick
    | Initialize Piece Piece
    | NextPiece Piece
    | NoOp
    | KeyUp Keyboard.Direction
    | KeyDown Keyboard.Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize currentPiece nextPiece ->
            ( { model
                | currentPiece = currentPiece
                , nextPiece = nextPiece
                , state = Initialized
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        Tick ->
            let
                newModel =
                    movePieceDown model
            in
            if detectCollisions newModel then
                ( fixateAndAdvance model, Random.generate NextPiece Piece.pieceGenerator )

            else
                ( newModel, Cmd.none )

        NextPiece piece ->
            ( { model | nextPiece = piece }, Cmd.none )

        KeyUp direction ->
            case direction of
                Keyboard.Down ->
                    ( { model | dropping = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyDown direction ->
            case direction of
                Keyboard.Up ->
                    ( rotateCurrentPiece model, Cmd.none )

                Keyboard.Right ->
                    ( moveCurrentPieceRight model, Cmd.none )

                Keyboard.Left ->
                    ( moveCurrentPieceLeft model, Cmd.none )

                Keyboard.Down ->
                    ( { model | dropping = True }, Cmd.none )

                Keyboard.Unknown ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        GameOver score ->
            text <| "Game over! Your score was " ++ String.fromInt score

        Uninitialized ->
            text ""

        Initialized ->
            div []
                [ Draw.renderOutline
                    |> Draw.pxSize
                , Draw.renderBoard model.currentPiece model.currentPiecePosition (Set.toList model.fixatedBlocks)
                    |> Draw.pxSize
                , Draw.renderNext model.nextPiece
                    |> Draw.pxSize
                , Draw.pixelWithItems
                    { x = (blockSize * 10) + 1
                    , y = blockSize * 0
                    , width = (blockSize * 5) + 1
                    , height = blockSize * 1
                    , color = "white"
                    }
                    [ div
                        [ style "margin-top" "5px"
                        , style "margin-left" (px (blockSize * 1))
                        ]
                        [ text ("SCORE: " ++ String.fromInt model.currentScore)
                        ]
                    ]
                ]

        Error error ->
            div [] [ text error ]


anyFixated : Model -> Int -> Bool
anyFixated state offset =
    let
        blocks =
            Piece.getBlocks state.currentPiece

        ( cx, cy ) =
            state.currentPiecePosition

        newBlocks : List ( Int, Int )
        newBlocks =
            List.map (\( x, y ) -> ( x + offset + cx, y + cy )) blocks

        fixatedBlocksXY : Set ( Int, Int )
        fixatedBlocksXY =
            state.fixatedBlocks
                |> Set.map (\( x, y, _ ) -> ( x, y ))

        blockIsFixated pos =
            Set.member pos fixatedBlocksXY
    in
    List.any blockIsFixated newBlocks


moveCurrentPieceLeft : Model -> Model
moveCurrentPieceLeft model =
    let
        ( x, y ) =
            model.currentPiecePosition

        left =
            Piece.getLeftOffset model.currentPiece
    in
    if x + left <= 0 then
        model

    else if anyFixated model -1 then
        model

    else
        { model | currentPiecePosition = ( x - 1, y ) }


moveCurrentPieceRight : Model -> Model
moveCurrentPieceRight model =
    let
        ( x, y ) =
            model.currentPiecePosition

        right =
            Piece.getRightOffset model.currentPiece
    in
    if x + 4 - right >= numCols then
        model

    else if anyFixated model 1 then
        model

    else
        { model | currentPiecePosition = ( x + 1, y ) }


rotateCurrentPiece : Model -> Model
rotateCurrentPiece model =
    let
        ( x, y ) =
            model.currentPiecePosition

        newPiece =
            Piece.rotate model.currentPiece
    in
    case Piece.getRight newPiece of
        Just right ->
            if x + right > numCols then
                { model | currentPiece = newPiece, currentPiecePosition = ( numCols - right, y ) }

            else
                { model | currentPiece = newPiece }

        Nothing ->
            Debug.todo "invalid right position!"


movePieceDown : Model -> Model
movePieceDown state =
    let
        ( x, y ) =
            state.currentPiecePosition
    in
    { state | currentPiecePosition = ( x, y + 1 ) }


translateRelativeTo : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
translateRelativeTo ( dx, dy ) ( x, y ) =
    ( dx + x, dy + y )


detectCollisions : Model -> Bool
detectCollisions state =
    let
        pieceBlocks =
            List.map (translateRelativeTo state.currentPiecePosition) <| Piece.getBlocks state.currentPiece

        fixatedBlocksXY =
            state.fixatedBlocks
                |> Set.map (\( x, y, _ ) -> ( x, y ))
    in
    List.any (\( _, y ) -> y >= numRows) pieceBlocks || List.any (\point -> Set.member point fixatedBlocksXY) pieceBlocks


fixate : Model -> Model
fixate state =
    let
        pieceBlocks =
            state.currentPiece
                |> Piece.getBlocks
                |> List.map (translateRelativeTo state.currentPiecePosition)
                |> List.map (\( x, y ) -> ( x, y, Piece.getColor state.currentPiece ))
    in
    { state | fixatedBlocks = Set.union state.fixatedBlocks <| Set.fromList pieceBlocks }


countBlocksByRow : List ( Int, Int ) -> List ( Int, Int )
countBlocksByRow blocks =
    let
        incrementCount ( _, row ) countDict =
            Dict.update row (Just << (+) 1 << Maybe.withDefault 0) countDict
    in
    Dict.toList <| List.foldl incrementCount Dict.empty blocks


checkForCompleteRows : Model -> Model
checkForCompleteRows state =
    let
        blockCounts =
            state.fixatedBlocks
                |> Set.toList
                |> List.map (\( x, y, _ ) -> ( x, y ))
                |> countBlocksByRow

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
    in
    case List.maximum <| Set.toList completeRows of
        Nothing ->
            state

        Just maxCompletedRow ->
            let
                completedRowsRemoved : List ( Int, Int, String )
                completedRowsRemoved =
                    state.fixatedBlocks
                        |> Set.toList
                        |> List.filter (\( _, row, _ ) -> not <| Set.member row completeRows)

                shiftDown ( x, row, color ) =
                    if row < maxCompletedRow then
                        ( x, row + Set.size completeRows, color )

                    else
                        ( x, row, color )

                shiftedRows =
                    List.map shiftDown completedRowsRemoved
            in
            { state | fixatedBlocks = Set.fromList <| shiftedRows, currentScore = state.currentScore + 100 * Set.size completeRows }


advance : Model -> Model
advance state =
    { state | currentPiece = state.nextPiece, currentPiecePosition = ( 0, 0 ) }


fixateAndAdvance : Model -> Model
fixateAndAdvance model =
    if detectCollisions model then
        { model | state = GameOver model.currentScore }

    else
        advance <| checkForCompleteRows <| fixate <| model


calcGameSpeed : Model -> Float
calcGameSpeed state =
    1000.0 - (toFloat state.currentScore / 8.0)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        msPerTick =
            calcGameSpeed model
                |> clamp 250.0 1000.0

        tickInterval =
            if model.dropping then
                msPerTick / 20

            else
                msPerTick
    in
    Sub.batch
        [ Time.every tickInterval <| always Tick
        , Browser.Events.onKeyDown (Json.map KeyDown Keyboard.keyDecoder)
        , Browser.Events.onKeyUp (Json.map KeyUp Keyboard.keyDecoder)
        ]
