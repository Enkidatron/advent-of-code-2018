module Day12 exposing (Rule, advanceXGenerations, aliveParser, doesPotHavePlant, hashGeneration, initialState, initialStateString, part1Answer, part2Answer, ruleMatches, ruleParser, ruleStrings, rules, scoreGeneration, stepGeneration, unsafeMaybe, whenDoWeCycle)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


initialStateString =
    "##..#.#.#..##..#..##..##..#.#....#.....##.#########...#.#..#..#....#.###.###....#..........###.#.#.."


ruleStrings =
    """..##. => .
..... => .
##..# => .
...#. => .
#.... => .
...## => #
.#.#. => .
#..#. => #
##.#. => .
#..## => .
..#.. => .
#.#.# => .
###.# => .
###.. => .
.#... => #
.##.# => .
##... => #
..### => .
####. => .
#...# => #
.#..# => #
##### => #
..#.# => #
.#.## => #
#.### => .
....# => .
.###. => .
.#### => #
.##.. => .
##.## => #
#.##. => #
#.#.. => #"""


type alias Rule =
    { twoLeft : Bool
    , oneLeft : Bool
    , current : Bool
    , oneRight : Bool
    , twoRight : Bool
    , output : Bool
    }


ruleParser : Parser Rule
ruleParser =
    Parser.succeed Rule
        |= aliveParser
        |= aliveParser
        |= aliveParser
        |= aliveParser
        |= aliveParser
        |. Parser.symbol " => "
        |= aliveParser


aliveParser =
    Parser.map (\char -> char == "#") <|
        Parser.getChompedString <|
            Parser.chompIf (always True)


rules =
    ruleStrings |> String.lines |> List.filterMap (Parser.run ruleParser >> Result.toMaybe)


initialState =
    initialStateString
        |> String.split ""
        |> List.map (\char -> char == "#")
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


stepGeneration lastGen =
    let
        min =
            Dict.toList lastGen
                |> List.filter Tuple.second
                |> List.map Tuple.first
                |> List.minimum
                |> Maybe.withDefault 0
                |> (\x -> x - 2)

        max =
            Dict.toList lastGen
                |> List.filter Tuple.second
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\x -> x + 2)

        pots =
            List.range min max
    in
    pots
        |> List.map (doesPotHavePlant lastGen)
        |> Dict.fromList


doesPotHavePlant : Dict Int Bool -> Int -> ( Int, Bool )
doesPotHavePlant lastGen potNumber =
    let
        havePlant =
            rules
                |> List.filter (ruleMatches lastGen potNumber)
                |> List.head
                |> Maybe.map .output
                |> unsafeMaybe
    in
    ( potNumber, havePlant )


unsafeMaybe maybe =
    case maybe of
        Just thing ->
            thing

        Nothing ->
            Debug.todo "unsafeMaybe"


ruleMatches : Dict Int Bool -> Int -> Rule -> Bool
ruleMatches lastGen potNum rule =
    (Dict.get (potNum - 2) lastGen |> Maybe.withDefault False |> (==) rule.twoLeft)
        && (Dict.get (potNum - 1) lastGen |> Maybe.withDefault False |> (==) rule.oneLeft)
        && (Dict.get potNum lastGen |> Maybe.withDefault False |> (==) rule.current)
        && (Dict.get (potNum + 1) lastGen |> Maybe.withDefault False |> (==) rule.oneRight)
        && (Dict.get (potNum + 2) lastGen |> Maybe.withDefault False |> (==) rule.twoRight)


advanceXGenerations generation x =
    if x <= 0 then
        generation

    else
        advanceXGenerations (stepGeneration generation) (x - 1)


part1Answer =
    advanceXGenerations initialState 20
        |> scoreGeneration


scoreGeneration =
    Dict.toList >> List.filter Tuple.second >> List.map Tuple.first >> List.sum


whenDoWeCycle generation seenHashes x =
    let
        generationHash =
            if remainderBy 100 x == 0 then
                Debug.log "heartbeat" (hashGeneration generation)

            else
                hashGeneration generation

        -- this is not used in the answer, but watching the heartbeat values
        -- is what led to the insight to come up with the score advancing shortcut
    in
    if Set.member generationHash seenHashes then
        x

    else
        whenDoWeCycle (stepGeneration generation)
            (Set.insert generationHash seenHashes)
            (x + 1)


hashGeneration generation =
    Dict.toList generation
        |> List.filter Tuple.second
        |> List.map (Tuple.first >> String.fromInt)
        |> String.join ","


advanceXGenerationsScoreShortcut x score =
    -- determined by manually comparing the scores for generations 1000 vs 1100, etc.
    -- After the pattern settles in (in the first 1000 generations),
    -- the generations gain score at a rate of 2300 per 100 generations
    -- we can shorten that to 23 / generation, as long as x is divisible by 100
    score + (23 * x)


part2Answer =
    -- We give it 1000 generations to settle into the pattern,
    -- then score it and use the score adjusting shortcut.
    advanceXGenerations initialState 1000
        |> scoreGeneration
        |> advanceXGenerationsScoreShortcut (50000000000 - 1000)
