module Day03 exposing (..)

import Day03Input exposing (..)
import Parser exposing (Parser, (|=), (|.))
import List.Extra
import Exts.Dict
import Dict exposing (Dict)


type alias Claim =
    { id : Int
    , startX : Int
    , startY : Int
    , width : Int
    , height : Int
    }


claimParser : Parser Claim
claimParser =
    Parser.succeed Claim
        |. Parser.symbol "#"
        |= Parser.int
        |. Parser.symbol " @ "
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int
        |. Parser.symbol ": "
        |= Parser.int
        |. Parser.symbol "x"
        |= Parser.int


claimsParser : Parser (List Claim)
claimsParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.succeed ()
        , item = claimParser
        , trailing = Parser.Optional
        }


claims : List Claim
claims =
    Parser.run claimsParser inputText
        |> Result.withDefault []


claimToPointList : Claim -> List ( Int, Int )
claimToPointList claim =
    let
        xs =
            List.range (claim.startX + 1) (claim.startX + claim.width)

        ys =
            List.range (claim.startY + 1) (claim.startY + claim.height)
    in
        List.Extra.cartesianProduct [ xs, ys ]
            |> List.map listToTupleUnsafe


listToTupleUnsafe : List a -> ( a, a )
listToTupleUnsafe list =
    case list of
        first :: second :: [] ->
            ( first, second )

        _ ->
            Debug.todo "told you it was unsafe"


part1Answer =
    pointClaimMap
        |> Dict.filter (\k freq -> freq > 1)
        |> Dict.size


pointClaimMap =
    claims |> List.concatMap claimToPointList |> Exts.Dict.frequency


claimDoesNotOverlap : Dict ( Int, Int ) Int -> Claim -> Bool
claimDoesNotOverlap pointMap claim =
    let
        claimPoints =
            claim |> claimToPointList
    in
        claimPoints |> List.all (pointDoesNotOverlap pointMap)


pointDoesNotOverlap pointMap point =
    let
        pointCount =
            Dict.get point pointMap |> Maybe.withDefault (0)
    in
        pointCount == 1


part2Answer =
    List.filter (claimDoesNotOverlap pointClaimMap) claims
