module Day06 exposing (Point, bottomWallInfinites, countClosestIfNoTie, findClosestInputPoint, getInfinitesFromWall, getTotalDistanceForPoint, infinites, inputText, leftWall, leftWallInfinites, listToPointUnsafe, makePoint, manhattanDistance, part1Answer, part2Answer, points, rightWallInfinites, topWallInfinites)

import Dict
import Exts.Dict
import List.Extra
import Set


type alias Point =
    ( Int, Int )


makePoint x y =
    ( x, y )


points =
    inputText
        |> String.lines
        |> List.map (String.split ", " >> List.filterMap String.toInt >> listToPointUnsafe)


listToPointUnsafe : List Int -> Point
listToPointUnsafe list =
    case list of
        x :: y :: [] ->
            makePoint x y

        _ ->
            Debug.todo "Yep, unsafe"


manhattanDistance : Point -> Point -> Int
manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


findClosestInputPoint : Point -> Maybe Point
findClosestInputPoint point =
    List.sortBy (manhattanDistance point) points
        |> countClosestIfNoTie point


countClosestIfNoTie point sortedPoints =
    case sortedPoints of
        [] ->
            Nothing

        firstPoint :: [] ->
            Just firstPoint

        firstPoint :: secondPoint :: _ ->
            if manhattanDistance point firstPoint == manhattanDistance point secondPoint then
                Nothing

            else
                Just firstPoint


getInfinitesFromWall wall =
    wall
        |> List.filterMap findClosestInputPoint
        |> Set.fromList


leftWallInfinites =
    List.range -500 1000
        |> List.map (\y -> makePoint -500 y)
        |> getInfinitesFromWall


rightWallInfinites =
    List.range -500 1000
        |> List.map (\y -> makePoint 1000 y)
        |> getInfinitesFromWall


topWallInfinites =
    List.range -500 1000
        |> List.map (\x -> makePoint x 1000)
        |> getInfinitesFromWall


bottomWallInfinites =
    List.range -500 1000
        |> List.map (\x -> makePoint x -500)
        |> getInfinitesFromWall


infinites =
    Set.union
        (Set.union leftWallInfinites rightWallInfinites)
        (Set.union topWallInfinites bottomWallInfinites)


part1Answer =
    List.Extra.cartesianProduct [ List.range 0 500, List.range 0 500 ]
        |> List.map listToPointUnsafe
        |> List.filterMap
            (\point ->
                findClosestInputPoint point
            )
        |> List.filter (\inputPoint -> not (Set.member inputPoint infinites))
        |> Exts.Dict.frequency
        |> Dict.toList
        |> List.Extra.maximumBy Tuple.second


getTotalDistanceForPoint : Point -> Int
getTotalDistanceForPoint point =
    List.map (manhattanDistance point) points
        |> List.sum


part2Answer =
    List.Extra.cartesianProduct [ List.range 0 500, List.range 0 500 ]
        |> List.map listToPointUnsafe
        |> List.map getTotalDistanceForPoint
        |> List.filter (\totalDistance -> totalDistance < 10000)
        |> List.length


inputText =
    """275, 276
176, 108
270, 134
192, 224
252, 104
240, 271
144, 220
341, 303
344, 166
142, 347
207, 135
142, 353
343, 74
90, 210
82, 236
124, 295
41, 226
298, 109
276, 314
50, 303
131, 42
119, 335
275, 125
113, 289
347, 230
192, 329
158, 316
154, 356
171, 350
165, 59
257, 129
306, 55
334, 203
55, 63
268, 198
44, 103
230, 199
41, 181
357, 328
331, 85
256, 290
168, 290
353, 77
81, 328
136, 316
138, 213
352, 271
139, 222
139, 318
194, 239"""
