module Day07 exposing
    ( Requirement
    , State
    , State2
    , Step
    , completeRequirements
    , decrementWorkTime
    , initialState
    , initialState2
    , inputText
    , lineToRequirement
    , notBlocked
    , notBlocked2
    , part1Answer
    , part1Sort
    , part2Answer
    , part2Test
    , requirements
    , simulate2
    , steps
    , steps2
    , stringToStep
    )

import Char
import List.Extra
import Set exposing (Set)


type alias Requirement =
    { dependent : String
    , prereq : String
    }


lineToRequirement : String -> Requirement
lineToRequirement line =
    Requirement (String.slice 36 37 line) (String.slice 5 6 line)


requirements : List Requirement
requirements =
    inputText
        |> String.lines
        |> List.map lineToRequirement


steps : List String
steps =
    List.map .dependent requirements
        ++ List.map .prereq requirements
        |> Set.fromList
        |> Set.toList
        |> List.sort


type alias State =
    { sortedReversedSteps : List String
    , requirementsLeft : List Requirement
    , stepsLeft : List String
    }


initialState =
    { sortedReversedSteps = []
    , requirementsLeft = requirements
    , stepsLeft = steps
    }


part1Sort : State -> State
part1Sort state =
    let
        nextStep =
            state.stepsLeft
                |> List.filter (notBlocked state.requirementsLeft)
                |> List.head
    in
    case nextStep of
        Nothing ->
            state

        Just step ->
            part1Sort
                { sortedReversedSteps = step :: state.sortedReversedSteps
                , requirementsLeft = completeRequirements step state.requirementsLeft
                , stepsLeft = List.Extra.remove step state.stepsLeft
                }


notBlocked : List Requirement -> String -> Bool
notBlocked reqs step =
    List.filter (\req -> req.dependent == step) reqs
        |> (\filtered -> filtered == [])


completeRequirements : String -> List Requirement -> List Requirement
completeRequirements completedStep reqs =
    List.filter (\req -> req.prereq /= completedStep) reqs


part1Answer =
    part1Sort initialState
        |> .sortedReversedSteps
        |> List.reverse
        |> String.join ""


type alias Step =
    { name : String
    , time : Int
    }


stringToStep : String -> Maybe Step
stringToStep letter =
    case String.toList letter |> List.head of
        Nothing ->
            Nothing

        Just char ->
            Just
                { name = letter
                , time = Char.toCode char - 4
                }


steps2 : List Step
steps2 =
    List.filterMap stringToStep steps


type alias State2 =
    { requirementsLeft : List Requirement
    , stepsLeft : List Step
    , timePassed : Int
    , workQueue : List Step
    }


initialState2 =
    { requirementsLeft = requirements
    , stepsLeft = steps2
    , timePassed = 0
    , workQueue = []
    }


simulate2 : State2 -> State2
simulate2 state =
    let
        completedSteps =
            List.filter (\step -> step.time == 0) state.workQueue
                |> List.map .name

        incompleteSteps =
            List.filter (\step -> step.time /= 0) state.workQueue

        newReqsLeft =
            List.foldl completeRequirements state.requirementsLeft completedSteps

        slotsOpen =
            4 - List.length incompleteSteps

        nextStepsToAdd =
            if slotsOpen > 0 then
                state.stepsLeft
                    |> List.filter (notBlocked2 newReqsLeft)
                    |> List.filter (notInWorkQueue incompleteSteps)
                    |> List.take slotsOpen

            else
                []

        nextStepNames =
            nextStepsToAdd |> List.map .name

        nextWorkQueue =
            incompleteSteps ++ nextStepsToAdd |> List.map decrementWorkTime

        _ =
            Debug.log "state" state
    in
    case nextWorkQueue of
        [] ->
            state

        _ ->
            simulate2
                { requirementsLeft = newReqsLeft
                , stepsLeft = List.filter (\step -> not <| List.member step.name nextStepNames) state.stepsLeft
                , timePassed = state.timePassed + 1
                , workQueue = nextWorkQueue
                }


notBlocked2 : List Requirement -> Step -> Bool
notBlocked2 reqs step =
    notBlocked reqs step.name


notInWorkQueue : List Step -> Step -> Bool
notInWorkQueue workQueue step =
    List.filter (\wipStep -> wipStep.name == step.name) workQueue
        |> (\list -> list == [])


decrementWorkTime : Step -> Step
decrementWorkTime step =
    { step | time = step.time - 1 }


part2Answer =
    simulate2 initialState2
        |> .timePassed


part2Test =
    { requirementsLeft = [ Requirement "C" "A" ]
    , workQueue = []
    , timePassed = 0
    , stepsLeft = [ Step "A" 10, Step "B" 15, Step "C" 20 ]
    }
        |> simulate2
        |> .timePassed


inputText =
    """Step S must be finished before step C can begin.
Step C must be finished before step R can begin.
Step L must be finished before step W can begin.
Step V must be finished before step B can begin.
Step P must be finished before step Y can begin.
Step M must be finished before step B can begin.
Step Y must be finished before step J can begin.
Step W must be finished before step T can begin.
Step N must be finished before step I can begin.
Step H must be finished before step O can begin.
Step O must be finished before step T can begin.
Step Q must be finished before step X can begin.
Step T must be finished before step K can begin.
Step A must be finished before step D can begin.
Step G must be finished before step K can begin.
Step D must be finished before step X can begin.
Step R must be finished before step J can begin.
Step U must be finished before step B can begin.
Step K must be finished before step J can begin.
Step B must be finished before step J can begin.
Step J must be finished before step E can begin.
Step E must be finished before step Z can begin.
Step F must be finished before step I can begin.
Step X must be finished before step Z can begin.
Step Z must be finished before step I can begin.
Step E must be finished before step F can begin.
Step R must be finished before step I can begin.
Step L must be finished before step Z can begin.
Step N must be finished before step O can begin.
Step O must be finished before step D can begin.
Step K must be finished before step I can begin.
Step R must be finished before step F can begin.
Step T must be finished before step F can begin.
Step N must be finished before step G can begin.
Step M must be finished before step D can begin.
Step F must be finished before step X can begin.
Step S must be finished before step D can begin.
Step Q must be finished before step F can begin.
Step L must be finished before step R can begin.
Step J must be finished before step F can begin.
Step L must be finished before step T can begin.
Step M must be finished before step H can begin.
Step D must be finished before step F can begin.
Step W must be finished before step B can begin.
Step C must be finished before step A can begin.
Step E must be finished before step I can begin.
Step P must be finished before step Q can begin.
Step A must be finished before step B can begin.
Step P must be finished before step R can begin.
Step C must be finished before step J can begin.
Step Y must be finished before step K can begin.
Step C must be finished before step L can begin.
Step E must be finished before step X can begin.
Step X must be finished before step I can begin.
Step A must be finished before step G can begin.
Step M must be finished before step E can begin.
Step C must be finished before step T can begin.
Step C must be finished before step Y can begin.
Step K must be finished before step E can begin.
Step H must be finished before step D can begin.
Step P must be finished before step K can begin.
Step D must be finished before step R can begin.
Step J must be finished before step X can begin.
Step H must be finished before step Z can begin.
Step M must be finished before step R can begin.
Step V must be finished before step U can begin.
Step K must be finished before step B can begin.
Step L must be finished before step Q can begin.
Step Y must be finished before step I can begin.
Step T must be finished before step G can begin.
Step U must be finished before step E can begin.
Step S must be finished before step Q can begin.
Step P must be finished before step G can begin.
Step P must be finished before step M can begin.
Step N must be finished before step J can begin.
Step P must be finished before step O can begin.
Step U must be finished before step J can begin.
Step C must be finished before step N can begin.
Step W must be finished before step R can begin.
Step B must be finished before step Z can begin.
Step F must be finished before step Z can begin.
Step O must be finished before step E can begin.
Step W must be finished before step N can begin.
Step A must be finished before step I can begin.
Step W must be finished before step J can begin.
Step R must be finished before step E can begin.
Step N must be finished before step B can begin.
Step M must be finished before step U can begin.
Step B must be finished before step E can begin.
Step V must be finished before step J can begin.
Step O must be finished before step I can begin.
Step Q must be finished before step T can begin.
Step Q must be finished before step U can begin.
Step L must be finished before step V can begin.
Step S must be finished before step Z can begin.
Step C must be finished before step P can begin.
Step P must be finished before step A can begin.
Step S must be finished before step G can begin.
Step N must be finished before step H can begin.
Step V must be finished before step H can begin.
Step B must be finished before step I can begin."""
