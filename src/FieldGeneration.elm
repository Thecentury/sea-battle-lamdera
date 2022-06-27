module FieldGeneration exposing (fieldGenerator)

import List.Extra as List
import Random exposing (Generator, constant)
import Random.Extra as Random
import Types exposing (Cell(..), Coord, Field, ShipHealth(..), ShipSize(..), emptyField, getCell, maxFieldCoord, setCell)


type alias NumericalShipSize =
    Int


shipSizes : List ShipSize
shipSizes =
    [ Size4, Size3, Size3, Size2, Size2, Size2, Size1, Size1, Size1, Size1 ]


type Orientation
    = Horizontal
    | Vertical


cellFromSize : ShipSize -> Cell
cellFromSize size =
    Ship size Alive


numericalSize : ShipSize -> NumericalShipSize
numericalSize size =
    case size of
        Size4 ->
            4

        Size3 ->
            3

        Size2 ->
            2

        Size1 ->
            1

        UnknownSize ->
            -- todo it should not occur
            0


cellsToPlace : ShipSize -> List Cell
cellsToPlace shipSize =
    List.initialize (numericalSize shipSize) (always <| cellFromSize shipSize)


tryAddCells : List Cell -> Coord -> (Coord -> Coord) -> Field -> Maybe Field
tryAddCells cells coord nextCoord field =
    case cells of
        [] ->
            Just field

        cell :: rest ->
            getCell field coord
                |> Maybe.andThen
                    (\existingCell ->
                        if existingCell == Empty then
                            setCell coord cell field
                                |> Maybe.andThen (tryAddCells rest (nextCoord coord) nextCoord)

                        else
                            Nothing
                    )


tryPlaceShip : ShipSize -> Field -> Coord -> Orientation -> Maybe Field
tryPlaceShip size field coord orientation =
    let
        cells =
            cellsToPlace size

        nextCoord c =
            case orientation of
                Horizontal ->
                    { c | x = c.x + 1 }

                Vertical ->
                    { c | y = c.y + 1 }
    in
    -- todo validate that ships are not too close
    tryAddCells cells coord nextCoord field


placeShipGenerator : Field -> ShipSize -> Generator Field
placeShipGenerator field size =
    Random.map2 (tryPlaceShip size field) positionGenerator orientationGenerator
        |> Random.andThen
            (\mField ->
                case mField of
                    Nothing ->
                        placeShipGenerator field size

                    Just newField ->
                        constant newField
            )


orientationGenerator : Generator Orientation
orientationGenerator =
    Random.choice Horizontal Vertical


positionGenerator : Generator Coord
positionGenerator =
    Random.map2 (\x y -> Coord x y)
        (Random.int 0 maxFieldCoord)
        (Random.int 0 maxFieldCoord)


fieldGeneratorCore : List ShipSize -> Field -> Generator Field
fieldGeneratorCore remainingSizes field =
    case remainingSizes of
        [] ->
            constant field

        size :: sizes ->
            placeShipGenerator field size
                |> Random.andThen (fieldGeneratorCore sizes)


fieldGenerator : Generator Field
fieldGenerator =
    fieldGeneratorCore shipSizes emptyField
