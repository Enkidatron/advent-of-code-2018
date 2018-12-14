module Day14Alt exposing (State, inputNumber, makePattern, makeState, part1, part2, part2BlankState, patternMatchesNewParts, recursePart2, recursivelyExpandState, scorePart1, stepState, stepState2)

import Array exposing (Array)


inputNumber =
    505961


type alias State =
    { recipes : Array Int
    , numRecipes : Int
    , firstElfIndex : Int
    , secondElfIndex : Int
    }


makeState number =
    { recipes = Array.repeat (number + 10) 0 |> Array.set 0 3 |> Array.set 1 7
    , numRecipes = 2
    , firstElfIndex = 0
    , secondElfIndex = 1
    }


recursivelyExpandState targetNum state =
    let
        newState =
            stepState state
    in
    if newState.numRecipes >= targetNum then
        newState

    else
        recursivelyExpandState targetNum newState


stepState state =
    let
        firstElfScore =
            Array.get state.firstElfIndex state.recipes
                |> Maybe.withDefault 0

        secondElfScore =
            Array.get state.secondElfIndex state.recipes
                |> Maybe.withDefault 0

        totalScore =
            firstElfScore + secondElfScore

        newRecipeScores =
            totalScore
                |> String.fromInt
                |> String.split ""
                |> List.filterMap String.toInt

        newRecipes =
            case newRecipeScores of
                firstScore :: secondScore :: _ ->
                    state.recipes
                        |> Array.set state.numRecipes firstScore
                        |> Array.set (state.numRecipes + 1) secondScore

                firstScore :: [] ->
                    state.recipes
                        |> Array.set state.numRecipes firstScore

                [] ->
                    state.recipes

        newNumRecipes =
            state.numRecipes + List.length newRecipeScores

        newFirstElfIndex =
            Basics.remainderBy newNumRecipes (state.firstElfIndex + 1 + firstElfScore)

        newSecondElfIndex =
            Basics.remainderBy newNumRecipes (state.secondElfIndex + 1 + secondElfScore)
    in
    { recipes = newRecipes
    , numRecipes = newNumRecipes
    , firstElfIndex = newFirstElfIndex
    , secondElfIndex = newSecondElfIndex
    }


scorePart1 state =
    Array.slice -10 (Array.length state.recipes) state.recipes
        |> Array.toList
        |> List.map String.fromInt
        |> String.join ""


part1 number =
    makeState number |> recursivelyExpandState (number + 10) |> scorePart1


part2 number =
    part2BlankState |> recursePart2 (makePattern number)


part2BlankState =
    { recipes = Array.repeat 1000000 0 |> Array.set 0 3 |> Array.set 1 7
    , numRecipes = 2
    , firstElfIndex = 0
    , secondElfIndex = 1
    }


makePattern number =
    String.fromInt number
        |> String.split ""
        |> List.filterMap String.toInt


recursePart2 targetPatternArray state =
    let
        newState =
            stepState2 state
    in
    case patternMatchesNewParts targetPatternArray newState of
        Just matchSize ->
            matchSize

        Nothing ->
            recursePart2 targetPatternArray newState


stepState2 state =
    let
        firstElfScore =
            Array.get state.firstElfIndex state.recipes
                |> Maybe.withDefault 0

        secondElfScore =
            Array.get state.secondElfIndex state.recipes
                |> Maybe.withDefault 0

        totalScore =
            firstElfScore + secondElfScore

        newRecipeScores =
            totalScore
                |> String.fromInt
                |> String.split ""
                |> List.filterMap String.toInt

        newNumRecipes =
            state.numRecipes + List.length newRecipeScores

        resizedRecipes =
            if newNumRecipes >= Array.length state.recipes then
                Array.append state.recipes (Array.repeat 1000000 0)

            else
                state.recipes

        _ =
            if Basics.remainderBy 10000 newNumRecipes == 0 then
                Debug.log "number of recipes" newNumRecipes

            else
                newNumRecipes

        newRecipes =
            case newRecipeScores of
                firstScore :: secondScore :: _ ->
                    resizedRecipes
                        |> Array.set state.numRecipes firstScore
                        |> Array.set (state.numRecipes + 1) secondScore

                firstScore :: [] ->
                    resizedRecipes
                        |> Array.set state.numRecipes firstScore

                [] ->
                    resizedRecipes

        newFirstElfIndex =
            Basics.remainderBy newNumRecipes (state.firstElfIndex + 1 + firstElfScore)

        newSecondElfIndex =
            Basics.remainderBy newNumRecipes (state.secondElfIndex + 1 + secondElfScore)
    in
    { recipes = newRecipes
    , numRecipes = newNumRecipes
    , firstElfIndex = newFirstElfIndex
    , secondElfIndex = newSecondElfIndex
    }


patternMatchesNewParts pattern state =
    let
        patternSize =
            List.length pattern

        patternMatchesAtOffset offset =
            List.indexedMap
                (\index patternValue ->
                    Array.get (state.numRecipes + index - offset) state.recipes
                        |> Maybe.withDefault -1
                        |> (\x -> x == patternValue)
                )
                pattern
                |> List.all identity
    in
    if patternMatchesAtOffset patternSize then
        Just (state.numRecipes - patternSize)

    else if patternMatchesAtOffset (patternSize + 1) then
        Just (state.numRecipes - (patternSize + 1))

    else
        Nothing
