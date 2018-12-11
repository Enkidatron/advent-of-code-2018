module Day11 exposing (PowerGrid, findBestSquareForSize, getPowerFromGrid, inputSerialNumber, makeCoarseGrid, makePowerGrid, makeVariableGrid, makeVariableGridCell, part1, part2, pointToPower)

import Array exposing (Array)
import List.Extra


inputSerialNumber =
    7347


pointToPower : Int -> Int -> Int -> Int
pointToPower serialNumber x y =
    let
        rackId =
            10 + x

        initialPower =
            rackId * y

        serialPower =
            serialNumber + initialPower

        multipliedPower =
            serialPower * rackId

        hundreds =
            Basics.remainderBy 10 (multipliedPower // 100)

        finalPower =
            hundreds - 5
    in
    finalPower


type alias PowerGrid =
    Array (Array Int)


makePowerGrid : Int -> PowerGrid
makePowerGrid serialNumber =
    Array.initialize 300 (makePowerGridRow serialNumber)


makePowerGridRow : Int -> Int -> Array Int
makePowerGridRow serialNumber yIndex =
    Array.initialize 300 (\xIndex -> pointToPower serialNumber (xIndex + 1) (yIndex + 1))


part1 serialNumber =
    let
        powerGrid =
            makePowerGrid serialNumber

        coarseGrid =
            makeCoarseGrid powerGrid
    in
    coarseGrid
        |> gridToList
        |> List.Extra.maximumBy Tuple.second
        |> Maybe.map Tuple.first


gridToList : PowerGrid -> List ( ( Int, Int ), Int )
gridToList =
    Array.toIndexedList
        >> List.concatMap
            (\( y, row ) ->
                Array.toIndexedList row
                    |> List.map (\( x, power ) -> ( ( x + 1, y + 1 ), power ))
            )


makeCoarseGrid : PowerGrid -> PowerGrid
makeCoarseGrid powerGrid =
    makeVariableGrid powerGrid 3



-- makeCoarseGridRow : Array (Array Int) -> Int -> Array Int
-- makeCoarseGridRow powerGrid y =
--     Array.initialize 297 (makeCoarseGridCell powerGrid y)
-- makeCoarseGridCell : Array (Array Int) -> Int -> Int -> Int
-- makeCoarseGridCell powerGrid y x =
--     [ ( x, y )
--     , ( x + 1, y )
--     , ( x + 2, y )
--     , ( x, y + 1 )
--     , ( x + 1, y + 1 )
--     , ( x + 2, y + 1 )
--     , ( x, y + 2 )
--     , ( x + 1, y + 2 )
--     , ( x + 2, y + 2 )
--     ]
--         |> List.map (getPowerFromGrid powerGrid)
--         |> List.sum


getPowerFromGrid powerGrid ( x, y ) =
    Array.get y powerGrid
        |> Maybe.andThen (\row -> Array.get x row)
        |> Maybe.withDefault -10


part2 serialNumber =
    let
        powerGrid =
            makePowerGrid serialNumber
    in
    List.range 1 25
        -- Just a wild guess that the answer size was smaller than 25.
        -- I was right, for my input
        -- I did this because performance is not great.
        |> List.filterMap (\size -> findBestSquareForSize powerGrid size)
        |> List.Extra.maximumBy Tuple.second


findBestSquareForSize : PowerGrid -> Int -> Maybe ( ( Int, Int, Int ), Int )
findBestSquareForSize powerGrid size =
    let
        variableGrid =
            makeVariableGrid powerGrid size
    in
    variableGrid
        |> gridToList
        |> List.Extra.maximumBy Tuple.second
        |> Maybe.map (insertSizeIntoPoint size)


insertSizeIntoPoint : Int -> ( ( Int, Int ), Int ) -> ( ( Int, Int, Int ), Int )
insertSizeIntoPoint size ( ( x, y ), power ) =
    ( ( x, y, size + 1 ), power )


makeVariableGrid : PowerGrid -> Int -> PowerGrid
makeVariableGrid powerGrid size =
    Array.initialize (300 - size) (makeVariableGridRow powerGrid size)


makeVariableGridRow powerGrid size y =
    Array.initialize (300 - size) (makeVariableGridCell powerGrid size y)


makeVariableGridCell powerGrid size y x =
    List.range 0 size
        |> List.concatMap
            (\dy ->
                List.range 0 size
                    |> List.map
                        (\dx ->
                            getPowerFromGrid powerGrid ( x + dx, y + dy )
                        )
            )
        |> List.sum
