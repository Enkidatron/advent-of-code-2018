module Day18 exposing (LandMap, Terrain(..), charToTerrain, exampleText, findCycle, hashLandMap, inputText, neighborTerrainCount, parseLandMap, parseLandMapLine, part1, part2, scorePart1, stepLineOneMinute, stepOneMinute, stepTerrainOneMinute, stepXMinutes, terrainToChar, visualizeLand, visualizeLine)

import Array exposing (Array)
import Dict exposing (Dict)


type alias LandMap =
    Array (Array Terrain)


type Terrain
    = Trees
    | Open
    | Lumberyard


parseLandMap text =
    String.lines text
        |> List.map parseLandMapLine
        |> Array.fromList


parseLandMapLine line =
    String.toList line |> List.map charToTerrain |> Array.fromList


charToTerrain char =
    case char of
        '|' ->
            Trees

        '.' ->
            Open

        '#' ->
            Lumberyard

        _ ->
            Debug.todo "bad terrain character"


part1 text =
    parseLandMap text |> stepXMinutes 10 |> scorePart1


scorePart1 land =
    let
        landList =
            Array.toList land |> List.concatMap Array.toList

        treeCount =
            List.filter (\t -> t == Trees) landList |> List.length

        lumberyardCount =
            List.filter (\t -> t == Lumberyard) landList |> List.length
    in
    treeCount * lumberyardCount


stepOneMinute land =
    Array.indexedMap (stepLineOneMinute land) land


stepXMinutes x land =
    if x <= 0 then
        land

    else
        stepXMinutes (x - 1) (stepOneMinute land)


stepLineOneMinute land y line =
    Array.indexedMap (stepTerrainOneMinute land y) line


stepTerrainOneMinute : LandMap -> Int -> Int -> Terrain -> Terrain
stepTerrainOneMinute land y x terrain =
    case terrain of
        Trees ->
            if neighborTerrainCount Lumberyard ( x, y ) land >= 3 then
                Lumberyard

            else
                Trees

        Open ->
            if neighborTerrainCount Trees ( x, y ) land >= 3 then
                Trees

            else
                Open

        Lumberyard ->
            if (neighborTerrainCount Trees ( x, y ) land >= 1) && (neighborTerrainCount Lumberyard ( x, y ) land >= 1) then
                Lumberyard

            else
                Open


neighborTerrainCount : Terrain -> ( Int, Int ) -> LandMap -> Int
neighborTerrainCount terrainType ( x, y ) land =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]
        |> List.filterMap (\( eachX, eachY ) -> Array.get eachY land |> Maybe.andThen (Array.get eachX))
        |> List.filter (\t -> t == terrainType)
        |> List.length


visualizeLand : LandMap -> LandMap
visualizeLand land =
    let
        message =
            Array.toList land |> List.map visualizeLine |> String.join "\n"

        _ =
            Debug.log message ()
    in
    land


visualizeLine : Array Terrain -> String
visualizeLine line =
    Array.toList line |> List.map terrainToChar |> String.fromList


terrainToChar terrain =
    case terrain of
        Trees ->
            '|'

        Open ->
            '.'

        Lumberyard ->
            '#'


part2 text =
    parseLandMap text |> stepXMinutes numberToActuallyCompute |> scorePart1


hashLandMap land =
    Array.toList land |> List.map visualizeLine |> String.join "\n"


findCycle cyclesPassed land landMemory =
    let
        hashedLand =
            hashLandMap land
    in
    case Dict.get hashedLand landMemory of
        Just lastSeenCycle ->
            ( lastSeenCycle, cyclesPassed )

        Nothing ->
            findCycle (cyclesPassed + 1) (stepOneMinute land) (Dict.insert hashedLand cyclesPassed landMemory)


originalNumberPart2 =
    1000000000


numberToActuallyCompute =
    -- used findCycle to find when a cycle started, and how long it lasted.
    552


exampleText =
    """.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|."""


inputText =
    """#.|#||..#......##.#||..||....|.#.|.#...#.|..|||.|.
.#..#||..#...##|.|##|..#|..##..#.|...|..|.|##.....
.#|..#..|||.|.|...|..##|.|...|.......|..#.|.#.|...
|....##..#|.........||#|.|.#....#.|.#.#|......#|.#
|.#.|....|.|.#..##|..|...##...|....|.|..##..|.#.#|
...|.|...|....|###.....|.##.#...#........|........
||..||.#|.|.#.|...#....#.#..|#|#.###.|.|...|...|#.
|..|..#..#|....#|...##...#.||..#..#.|.|...#...|.|#
..#...|....|..|.|##...#.#.|#..#.|...#.#..#..#.#.|.
|#.|##.#....#.|.|||#.|#...#|.|#|#.###....|..|.|...
..||#..#..#.|.#...#.#..|.|...|.##|..|...#||....|..
||.|......|.#...##|..#.|.....##|.#..#.||...|.#|.|.
#...|....|..#.....|.#....||#||..|...#||........|#.
|.|....#...#|..#.....#..|..||#..|...#..|...#|.#...
..#|.#.##||#|.#...|...|...#.#||.....#|.|.|.|#|.|..
|..|#..|#...#..|#.|.#..|.#.#|...|.......##.|..##..
##..#|.#||......#...|..#.|.|..#.#...|...........|.
.#....#.|.#...|.#..|.###...|...##........###..|#.#
#......#||#..#..|..#..#|.#.|...||##..|.#.|###.##..
|#.|#......|...#..|#.#|.|.|.##.|#.|........#....#.
#.|.#.|..#..||...|..|#.|..|#|.#|...||.|...#||#|.|.
....#|..|...|##.#...#.||.|...|..#|#.......##...#..
..#..#..|..|...|.|#..|...|#...|..#..|.||.##.###...
.#...###...|#|....#|||..##......#|..#.#|..|#|.#|..
.||.|#....###|.#..##..|###.|...|.....#..|..#......
#.......#...||##..#...#..|..#...##.|#..|.|.#|.#...
|.....#|#....#...#.#.....|#....|#|#|##|#||....|.|#
......#|..#...#.|.....|.|...|.|#.|..|#.#.#.|..#...
|####......#.|.....|.|.....#..#.....|#.#|...#..#.|
||.............|....|||..#|#...##..#|.#.#|.#.|.#.|
..|.....#|.###..#|..#..||...|..|#|..|.||...#.|....
.####..#...#|.##..|#.||.#|#........|.|#|...#..|...
#.##.....#|...|...#.|###..|#|.....#...|..|.|#|.|.#
|.|##.|..|..#|#......#|#......#....#|||#...|#.#.#.
........|.|.#.|#...#.#.......|.|.#|...|#.......|#.
...#.##...#.###|#....||.|...#......|#...#.#...|.#.
|...|#..|.||...#.||.|##....#.##|..|.||.....#||....
#||..|.|......|...|||.#.#.#....|#...|#|.|...|.#..|
#.##.#....#|.|.|#...|..|####...#...|#...|....#....
#|..|||#|....|#|....||....#..#...|||#|.....#...#..
#|.|....||.#...#|.#.|....|...|..|#|.#.#.||..||.##|
|#.|.#...#|#.|...#.|.|..||.|.|..#.#||....#|.|##|..
....##|||#.#....#.##|.#.#|#|#.##....#|.....|..|...
#.|.....|.||.|.#|.#.#|#..##|.#|.##.#.#...#||||#...
.#|..||#...#|.#...|.#|.|.###...|.#....||.|...#..||
.......#...#...##|#....#.||#.....|.#..|..||||.....
.......#|..#......|.##..##..#.|.|||.|..|.##|#|#|#.
...#............#.##...|......#.||#..|.......##||.
.##||..#|##.....|||....|.......|.#.|.|.|...|..|..|
..#.|.#..#.#....#..#.|..||....#......##.|.#..#..#."""
