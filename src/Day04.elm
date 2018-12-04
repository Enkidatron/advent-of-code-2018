module Day04 exposing (DateStamp, Event(..), EventStamp, GuardId, State, blankState, calcGuardIdTimesMinute, compareDateStamp, compareEventStamp, eventLogReducer, eventParser, eventStampParser, eventStamps, findBiggestFrequency, findMostFrequentMinute, inputText, leadingZeroIntParser, makeEventStamp, part1Answer, part2Answer, processedLog, unsafeMaybe)

import Dict exposing (Dict)
import Exts.Dict
import Parser exposing ((|.), (|=), Parser)


type alias DateStamp =
    { month : Int
    , day : Int
    , hour : Int
    , minute : Int
    }


type Event
    = GuardChange Int
    | FallsAsleep
    | WakesUp


type alias EventStamp =
    { dateStamp : DateStamp
    , event : Event
    }


makeEventStamp : Int -> Int -> Int -> Int -> Event -> EventStamp
makeEventStamp month day hour minute event =
    { dateStamp = DateStamp month day hour minute
    , event = event
    }


leadingZeroIntParser : Parser Int
leadingZeroIntParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "0"
            |= Parser.int
        , Parser.int
        ]


eventStampParser : Parser EventStamp
eventStampParser =
    Parser.succeed makeEventStamp
        |. Parser.symbol "[1518-"
        |= leadingZeroIntParser
        |. Parser.symbol "-"
        |= leadingZeroIntParser
        |. Parser.symbol " "
        |= leadingZeroIntParser
        |. Parser.symbol ":"
        |= leadingZeroIntParser
        |. Parser.symbol "] "
        |= eventParser


eventParser : Parser Event
eventParser =
    Parser.oneOf
        [ Parser.succeed FallsAsleep
            |. Parser.keyword "falls asleep"
        , Parser.succeed WakesUp
            |. Parser.keyword "wakes up"
        , Parser.succeed GuardChange
            |. Parser.symbol "Guard #"
            |= Parser.int
            |. Parser.symbol " begins shift"
        ]


eventStamps =
    inputText
        |> String.lines
        |> List.filterMap (Parser.run eventStampParser >> Result.toMaybe)
        |> List.sortWith compareEventStamp


compareEventStamp a b =
    compareDateStamp a.dateStamp b.dateStamp


compareDateStamp : DateStamp -> DateStamp -> Basics.Order
compareDateStamp a b =
    case compare a.month b.month of
        EQ ->
            case compare a.day b.day of
                EQ ->
                    case compare a.hour b.hour of
                        EQ ->
                            compare a.minute b.minute

                        _ ->
                            compare a.hour b.hour

                _ ->
                    compare a.day b.day

        _ ->
            compare a.month b.month


type alias State =
    { guardLog : Dict GuardId (List Int)
    , sleepTime : Maybe Int
    , activeGuard : GuardId
    }


blankState =
    { guardLog = Dict.empty
    , sleepTime = Nothing
    , activeGuard = 0
    }


type alias GuardId =
    Int


eventLogReducer : EventStamp -> State -> State
eventLogReducer eventStamp state =
    case eventStamp.event of
        GuardChange guardId ->
            { state | activeGuard = guardId }

        FallsAsleep ->
            { state | sleepTime = Just eventStamp.dateStamp.minute }

        WakesUp ->
            let
                newMinutes =
                    case state.sleepTime of
                        Just sleepTime ->
                            List.range sleepTime (eventStamp.dateStamp.minute - 1)

                        Nothing ->
                            Debug.todo "Too much coffee"

                newLogEntry =
                    case Dict.get state.activeGuard state.guardLog of
                        Nothing ->
                            newMinutes

                        Just oldMinutes ->
                            oldMinutes ++ newMinutes
            in
            { state | sleepTime = Nothing, guardLog = Dict.insert state.activeGuard newLogEntry state.guardLog }


processedLog =
    List.foldl eventLogReducer blankState eventStamps
        |> .guardLog


part1Answer =
    processedLog
        |> Dict.toList
        |> List.sortBy (\( guardId, minutes ) -> List.length minutes)
        |> List.reverse
        |> List.head
        |> unsafeMaybe
        |> calcGuardIdTimesMinute


calcGuardIdTimesMinute ( guardId, minutes ) =
    let
        minute =
            findMostFrequentMinute minutes
    in
    minute * guardId


findMostFrequentMinute : List Int -> Int
findMostFrequentMinute minutes =
    Exts.Dict.frequency minutes
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.head
        |> unsafeMaybe
        |> Tuple.first


findBiggestFrequency : List Int -> Int
findBiggestFrequency minutes =
    Exts.Dict.frequency minutes
        |> Dict.values
        |> List.maximum
        |> Maybe.withDefault 0


unsafeMaybe : Maybe a -> a
unsafeMaybe maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo "unsafe Maybe"


part2Answer =
    processedLog
        |> Dict.toList
        |> List.sortBy (\( guardId, minutes ) -> findBiggestFrequency minutes)
        |> List.reverse
        |> List.head
        |> unsafeMaybe
        |> calcGuardIdTimesMinute


inputText =
    """[1518-04-21 00:57] wakes up
[1518-09-03 00:12] falls asleep
[1518-04-21 00:04] Guard #3331 begins shift
[1518-10-29 00:51] falls asleep
[1518-06-12 00:02] Guard #3109 begins shift
[1518-06-01 00:46] wakes up
[1518-03-07 00:02] Guard #653 begins shift
[1518-10-06 00:00] Guard #2339 begins shift
[1518-05-20 00:47] wakes up
[1518-10-08 00:58] wakes up
[1518-04-19 00:57] wakes up
[1518-04-16 00:28] falls asleep
[1518-04-12 00:33] wakes up
[1518-04-21 00:19] falls asleep
[1518-08-04 00:52] wakes up
[1518-07-09 00:02] falls asleep
[1518-03-24 00:22] wakes up
[1518-11-19 00:28] falls asleep
[1518-09-18 00:38] falls asleep
[1518-07-28 00:00] Guard #3331 begins shift
[1518-09-18 00:39] wakes up
[1518-07-13 23:59] Guard #653 begins shift
[1518-05-15 00:16] wakes up
[1518-03-31 00:53] wakes up
[1518-05-06 23:46] Guard #2543 begins shift
[1518-04-09 00:28] falls asleep
[1518-04-12 00:23] falls asleep
[1518-05-18 00:50] wakes up
[1518-03-16 00:01] Guard #1117 begins shift
[1518-06-13 00:19] falls asleep
[1518-09-15 00:45] wakes up
[1518-03-23 00:58] wakes up
[1518-08-25 00:18] falls asleep
[1518-03-16 00:23] falls asleep
[1518-10-16 00:41] wakes up
[1518-11-09 00:37] wakes up
[1518-07-05 00:03] falls asleep
[1518-09-24 00:16] falls asleep
[1518-05-18 23:49] Guard #3079 begins shift
[1518-09-03 00:28] wakes up
[1518-03-12 00:47] falls asleep
[1518-09-01 00:47] wakes up
[1518-06-23 23:48] Guard #971 begins shift
[1518-03-27 00:34] wakes up
[1518-06-18 00:40] falls asleep
[1518-03-02 00:48] wakes up
[1518-04-11 00:48] wakes up
[1518-03-31 00:46] wakes up
[1518-08-07 00:57] wakes up
[1518-03-04 00:22] falls asleep
[1518-04-10 00:57] wakes up
[1518-05-16 00:00] Guard #2179 begins shift
[1518-06-06 00:14] wakes up
[1518-03-20 00:54] wakes up
[1518-08-01 00:55] wakes up
[1518-07-03 00:00] Guard #1069 begins shift
[1518-10-14 00:47] falls asleep
[1518-06-01 00:03] falls asleep
[1518-07-10 00:35] falls asleep
[1518-11-12 23:56] Guard #3461 begins shift
[1518-09-04 00:39] falls asleep
[1518-03-15 00:47] wakes up
[1518-04-10 00:30] wakes up
[1518-03-21 00:52] wakes up
[1518-05-09 00:03] Guard #1877 begins shift
[1518-09-29 00:45] wakes up
[1518-03-25 00:37] wakes up
[1518-10-11 00:57] wakes up
[1518-11-09 00:16] wakes up
[1518-08-10 00:18] falls asleep
[1518-06-24 00:30] falls asleep
[1518-08-01 00:54] falls asleep
[1518-08-14 00:12] falls asleep
[1518-10-21 00:46] wakes up
[1518-03-18 00:48] falls asleep
[1518-03-25 23:59] Guard #3331 begins shift
[1518-06-05 23:47] Guard #1117 begins shift
[1518-07-18 00:04] falls asleep
[1518-11-05 00:48] falls asleep
[1518-11-19 00:43] wakes up
[1518-04-16 00:39] wakes up
[1518-09-30 00:46] wakes up
[1518-10-31 00:01] Guard #3079 begins shift
[1518-07-13 00:11] falls asleep
[1518-07-05 00:47] wakes up
[1518-03-22 23:59] Guard #1877 begins shift
[1518-04-18 00:45] wakes up
[1518-08-15 00:49] falls asleep
[1518-04-03 23:57] Guard #2543 begins shift
[1518-07-08 00:29] falls asleep
[1518-06-21 00:41] wakes up
[1518-07-18 00:38] wakes up
[1518-04-29 00:20] wakes up
[1518-10-31 00:56] wakes up
[1518-08-05 00:26] wakes up
[1518-10-03 00:15] falls asleep
[1518-10-05 00:51] wakes up
[1518-04-19 00:43] falls asleep
[1518-10-18 00:30] falls asleep
[1518-10-03 00:04] Guard #2543 begins shift
[1518-03-15 00:40] falls asleep
[1518-06-15 00:16] falls asleep
[1518-06-11 00:00] Guard #1093 begins shift
[1518-05-13 00:00] Guard #919 begins shift
[1518-07-25 00:05] falls asleep
[1518-03-05 00:03] Guard #3079 begins shift
[1518-04-05 00:53] wakes up
[1518-06-16 23:56] Guard #727 begins shift
[1518-07-25 23:50] Guard #2339 begins shift
[1518-04-11 00:09] wakes up
[1518-11-14 00:27] falls asleep
[1518-10-28 00:57] wakes up
[1518-07-11 00:53] falls asleep
[1518-06-27 00:35] wakes up
[1518-10-23 23:47] Guard #3331 begins shift
[1518-11-10 00:36] falls asleep
[1518-08-13 00:46] wakes up
[1518-03-02 00:02] Guard #3079 begins shift
[1518-04-05 00:00] Guard #683 begins shift
[1518-10-17 00:28] wakes up
[1518-10-17 00:48] wakes up
[1518-06-25 00:18] wakes up
[1518-06-21 00:11] falls asleep
[1518-07-09 00:51] wakes up
[1518-03-28 00:45] wakes up
[1518-09-21 23:46] Guard #3079 begins shift
[1518-07-18 23:57] Guard #971 begins shift
[1518-03-22 00:29] falls asleep
[1518-06-24 00:49] wakes up
[1518-09-30 23:50] Guard #2179 begins shift
[1518-06-13 00:07] falls asleep
[1518-07-16 00:04] Guard #2179 begins shift
[1518-11-17 00:51] wakes up
[1518-04-28 00:29] falls asleep
[1518-11-11 00:54] wakes up
[1518-10-07 00:46] falls asleep
[1518-11-18 00:15] falls asleep
[1518-05-22 00:51] wakes up
[1518-03-22 00:54] falls asleep
[1518-10-29 00:04] Guard #1931 begins shift
[1518-10-22 00:21] falls asleep
[1518-07-26 00:02] falls asleep
[1518-03-27 00:54] wakes up
[1518-09-04 00:00] Guard #653 begins shift
[1518-06-24 00:05] falls asleep
[1518-10-15 00:53] wakes up
[1518-05-07 00:54] wakes up
[1518-07-30 23:59] Guard #2179 begins shift
[1518-05-07 00:37] falls asleep
[1518-05-09 00:07] falls asleep
[1518-07-03 00:47] wakes up
[1518-03-28 00:02] Guard #653 begins shift
[1518-08-11 00:19] falls asleep
[1518-05-19 23:59] Guard #2339 begins shift
[1518-10-21 00:56] wakes up
[1518-05-26 23:56] Guard #1117 begins shift
[1518-11-18 00:16] wakes up
[1518-10-11 00:24] wakes up
[1518-08-11 00:11] falls asleep
[1518-03-20 00:18] falls asleep
[1518-09-29 00:34] falls asleep
[1518-04-11 23:59] Guard #1069 begins shift
[1518-05-24 00:35] falls asleep
[1518-11-15 00:36] falls asleep
[1518-05-15 00:51] falls asleep
[1518-09-20 00:13] falls asleep
[1518-08-01 00:20] falls asleep
[1518-09-01 00:01] Guard #1877 begins shift
[1518-06-15 00:00] Guard #919 begins shift
[1518-08-09 00:00] Guard #2179 begins shift
[1518-03-12 00:11] falls asleep
[1518-04-23 00:01] Guard #3331 begins shift
[1518-03-23 00:43] falls asleep
[1518-03-14 00:00] falls asleep
[1518-10-29 00:52] wakes up
[1518-05-08 00:18] falls asleep
[1518-10-20 00:34] falls asleep
[1518-05-05 23:47] Guard #1093 begins shift
[1518-11-01 23:58] Guard #1409 begins shift
[1518-07-05 23:56] Guard #3319 begins shift
[1518-11-03 00:49] wakes up
[1518-09-12 00:59] wakes up
[1518-07-16 00:17] falls asleep
[1518-08-02 00:05] falls asleep
[1518-03-02 00:52] wakes up
[1518-03-04 00:46] wakes up
[1518-09-16 00:40] falls asleep
[1518-05-15 00:15] falls asleep
[1518-07-22 00:46] wakes up
[1518-05-05 00:16] falls asleep
[1518-03-10 23:53] Guard #3079 begins shift
[1518-03-14 00:42] wakes up
[1518-10-26 00:06] falls asleep
[1518-04-11 00:07] falls asleep
[1518-08-23 00:48] wakes up
[1518-05-06 00:49] wakes up
[1518-05-14 00:40] wakes up
[1518-06-02 23:51] Guard #1117 begins shift
[1518-03-09 00:53] wakes up
[1518-05-02 00:43] wakes up
[1518-08-06 00:01] Guard #2953 begins shift
[1518-03-01 00:54] wakes up
[1518-04-16 00:57] wakes up
[1518-09-08 00:02] Guard #727 begins shift
[1518-03-11 00:54] wakes up
[1518-03-30 00:17] falls asleep
[1518-11-22 00:53] falls asleep
[1518-11-07 00:41] wakes up
[1518-03-24 00:41] wakes up
[1518-03-10 00:47] wakes up
[1518-04-18 23:58] Guard #919 begins shift
[1518-08-01 00:51] wakes up
[1518-10-09 00:28] falls asleep
[1518-06-17 00:20] falls asleep
[1518-10-23 00:49] wakes up
[1518-07-14 00:18] falls asleep
[1518-08-31 00:20] falls asleep
[1518-06-11 00:56] falls asleep
[1518-10-22 00:00] Guard #3079 begins shift
[1518-06-08 00:24] wakes up
[1518-09-30 00:51] falls asleep
[1518-10-19 00:08] falls asleep
[1518-04-05 00:32] wakes up
[1518-05-13 00:46] wakes up
[1518-09-17 00:52] wakes up
[1518-04-17 00:50] wakes up
[1518-11-15 00:00] Guard #1877 begins shift
[1518-06-22 00:59] wakes up
[1518-07-25 00:46] wakes up
[1518-04-30 23:48] Guard #1877 begins shift
[1518-09-12 00:03] Guard #1069 begins shift
[1518-09-21 00:54] wakes up
[1518-07-10 00:56] wakes up
[1518-03-08 00:30] wakes up
[1518-07-13 00:52] wakes up
[1518-05-23 00:44] wakes up
[1518-10-28 00:51] falls asleep
[1518-06-25 00:03] Guard #919 begins shift
[1518-10-29 00:31] wakes up
[1518-10-10 00:25] wakes up
[1518-05-23 00:58] wakes up
[1518-05-14 00:52] falls asleep
[1518-10-04 00:55] falls asleep
[1518-09-20 23:56] Guard #3319 begins shift
[1518-03-17 23:50] Guard #1093 begins shift
[1518-03-08 00:52] falls asleep
[1518-06-26 00:33] falls asleep
[1518-05-30 00:58] wakes up
[1518-05-27 23:56] Guard #3461 begins shift
[1518-05-31 00:58] wakes up
[1518-11-11 23:56] Guard #3301 begins shift
[1518-03-11 00:15] wakes up
[1518-09-14 00:58] wakes up
[1518-10-15 00:21] falls asleep
[1518-11-06 00:06] falls asleep
[1518-10-28 00:00] Guard #1877 begins shift
[1518-08-18 00:54] falls asleep
[1518-04-07 00:49] falls asleep
[1518-06-27 00:46] falls asleep
[1518-04-14 00:53] falls asleep
[1518-03-11 00:41] falls asleep
[1518-07-08 00:39] wakes up
[1518-10-04 00:03] Guard #2179 begins shift
[1518-04-06 00:57] wakes up
[1518-09-03 00:36] falls asleep
[1518-06-12 00:24] falls asleep
[1518-08-11 23:58] Guard #727 begins shift
[1518-03-18 23:59] Guard #641 begins shift
[1518-11-09 00:01] Guard #2179 begins shift
[1518-05-31 00:52] falls asleep
[1518-05-24 00:57] wakes up
[1518-05-12 00:40] falls asleep
[1518-08-24 23:59] Guard #1931 begins shift
[1518-10-15 00:03] Guard #3319 begins shift
[1518-05-04 00:21] falls asleep
[1518-10-09 00:00] Guard #1069 begins shift
[1518-09-13 00:31] falls asleep
[1518-08-25 00:35] wakes up
[1518-04-20 00:17] falls asleep
[1518-06-10 00:56] wakes up
[1518-06-05 00:37] falls asleep
[1518-10-20 23:56] Guard #1931 begins shift
[1518-09-14 23:59] Guard #2179 begins shift
[1518-10-14 00:32] wakes up
[1518-04-11 00:03] Guard #2953 begins shift
[1518-03-24 00:00] Guard #3461 begins shift
[1518-03-08 00:47] wakes up
[1518-11-19 00:07] falls asleep
[1518-07-19 00:38] falls asleep
[1518-08-22 00:40] wakes up
[1518-06-07 00:04] falls asleep
[1518-04-07 00:59] wakes up
[1518-05-24 00:44] wakes up
[1518-08-08 00:02] falls asleep
[1518-09-20 00:37] wakes up
[1518-08-03 00:35] falls asleep
[1518-03-13 23:47] Guard #3461 begins shift
[1518-06-17 00:50] wakes up
[1518-08-24 00:51] wakes up
[1518-06-02 00:26] wakes up
[1518-06-16 00:41] falls asleep
[1518-08-11 00:42] falls asleep
[1518-07-22 00:41] falls asleep
[1518-11-07 00:58] wakes up
[1518-05-08 00:03] Guard #683 begins shift
[1518-05-13 00:08] falls asleep
[1518-05-25 00:55] wakes up
[1518-11-18 00:44] wakes up
[1518-06-30 00:32] wakes up
[1518-08-07 00:40] wakes up
[1518-07-13 00:00] Guard #3079 begins shift
[1518-04-01 00:40] wakes up
[1518-03-03 23:58] Guard #3319 begins shift
[1518-09-23 00:35] wakes up
[1518-08-28 00:57] wakes up
[1518-04-15 00:52] falls asleep
[1518-05-27 00:14] falls asleep
[1518-11-07 00:13] falls asleep
[1518-03-14 23:57] Guard #3109 begins shift
[1518-10-30 00:54] wakes up
[1518-10-30 00:00] Guard #727 begins shift
[1518-08-20 00:22] falls asleep
[1518-04-01 00:04] falls asleep
[1518-03-12 00:54] wakes up
[1518-03-07 00:44] wakes up
[1518-09-26 00:39] wakes up
[1518-03-06 00:49] wakes up
[1518-10-29 00:14] falls asleep
[1518-08-25 00:28] falls asleep
[1518-09-25 00:32] falls asleep
[1518-06-06 23:47] Guard #2339 begins shift
[1518-09-19 00:34] wakes up
[1518-08-16 00:50] wakes up
[1518-10-07 00:28] wakes up
[1518-11-17 00:21] falls asleep
[1518-07-28 00:14] falls asleep
[1518-09-13 00:50] falls asleep
[1518-09-14 00:26] falls asleep
[1518-09-18 23:56] Guard #919 begins shift
[1518-03-03 00:35] falls asleep
[1518-08-31 00:03] Guard #1093 begins shift
[1518-10-19 00:00] Guard #2339 begins shift
[1518-09-28 23:59] Guard #1877 begins shift
[1518-07-11 23:58] Guard #2179 begins shift
[1518-06-19 00:56] falls asleep
[1518-10-22 00:45] wakes up
[1518-09-02 00:49] falls asleep
[1518-04-08 00:48] falls asleep
[1518-07-19 00:58] wakes up
[1518-03-07 00:36] falls asleep
[1518-05-26 00:00] Guard #919 begins shift
[1518-03-15 00:53] falls asleep
[1518-03-02 23:58] Guard #653 begins shift
[1518-07-20 00:55] wakes up
[1518-08-28 00:48] falls asleep
[1518-05-21 00:10] wakes up
[1518-08-25 23:49] Guard #1931 begins shift
[1518-03-26 23:46] Guard #1877 begins shift
[1518-08-19 00:25] falls asleep
[1518-04-22 00:03] Guard #1277 begins shift
[1518-09-08 00:54] falls asleep
[1518-04-05 00:18] falls asleep
[1518-09-14 00:46] wakes up
[1518-08-03 00:47] wakes up
[1518-07-04 00:47] wakes up
[1518-11-11 00:21] falls asleep
[1518-09-05 00:42] falls asleep
[1518-08-16 00:41] falls asleep
[1518-05-06 00:34] falls asleep
[1518-08-12 00:54] wakes up
[1518-07-20 00:15] falls asleep
[1518-09-30 00:04] Guard #3079 begins shift
[1518-10-27 00:02] falls asleep
[1518-11-09 23:56] Guard #3109 begins shift
[1518-04-14 23:56] Guard #2953 begins shift
[1518-08-28 23:48] Guard #1877 begins shift
[1518-03-16 00:38] wakes up
[1518-10-01 00:39] wakes up
[1518-08-02 23:57] Guard #971 begins shift
[1518-04-20 00:20] wakes up
[1518-08-27 00:04] Guard #3301 begins shift
[1518-03-26 00:45] wakes up
[1518-09-16 00:57] wakes up
[1518-06-02 00:51] wakes up
[1518-06-19 00:59] wakes up
[1518-09-09 00:50] wakes up
[1518-05-02 00:14] falls asleep
[1518-04-10 00:50] falls asleep
[1518-09-08 00:51] wakes up
[1518-05-02 23:58] Guard #727 begins shift
[1518-05-14 00:04] Guard #919 begins shift
[1518-08-24 00:24] falls asleep
[1518-04-13 00:18] wakes up
[1518-04-10 00:45] wakes up
[1518-08-08 00:42] falls asleep
[1518-03-26 00:27] falls asleep
[1518-04-04 00:50] falls asleep
[1518-10-01 00:03] falls asleep
[1518-05-23 00:39] falls asleep
[1518-08-25 00:59] wakes up
[1518-04-27 23:57] Guard #1117 begins shift
[1518-10-04 00:39] wakes up
[1518-08-25 00:40] falls asleep
[1518-07-17 00:40] wakes up
[1518-11-03 00:01] Guard #919 begins shift
[1518-04-02 00:27] falls asleep
[1518-09-10 00:19] falls asleep
[1518-06-23 00:36] falls asleep
[1518-09-25 00:00] Guard #1931 begins shift
[1518-09-25 00:18] wakes up
[1518-08-12 00:47] falls asleep
[1518-04-23 00:56] wakes up
[1518-08-26 00:27] wakes up
[1518-11-16 00:29] wakes up
[1518-03-18 00:41] wakes up
[1518-06-18 00:00] Guard #2953 begins shift
[1518-11-13 00:49] wakes up
[1518-04-30 00:56] wakes up
[1518-03-28 23:57] Guard #2953 begins shift
[1518-04-06 23:59] Guard #3319 begins shift
[1518-08-14 00:59] wakes up
[1518-11-19 23:48] Guard #971 begins shift
[1518-04-10 00:43] falls asleep
[1518-06-24 00:23] wakes up
[1518-10-31 00:53] falls asleep
[1518-11-04 00:54] falls asleep
[1518-08-12 00:43] wakes up
[1518-03-13 00:52] falls asleep
[1518-07-08 23:48] Guard #3461 begins shift
[1518-11-05 00:57] wakes up
[1518-05-14 00:33] wakes up
[1518-07-24 00:58] wakes up
[1518-03-03 00:55] wakes up
[1518-05-21 00:45] falls asleep
[1518-03-12 23:58] Guard #3109 begins shift
[1518-06-27 00:07] falls asleep
[1518-10-17 00:46] falls asleep
[1518-07-21 00:55] wakes up
[1518-08-06 00:57] wakes up
[1518-09-23 00:02] falls asleep
[1518-07-14 23:58] Guard #2339 begins shift
[1518-08-07 00:00] Guard #3109 begins shift
[1518-03-14 00:19] falls asleep
[1518-11-21 00:28] falls asleep
[1518-08-21 00:55] wakes up
[1518-11-21 00:04] Guard #1931 begins shift
[1518-04-09 23:57] Guard #3079 begins shift
[1518-06-05 00:04] Guard #727 begins shift
[1518-07-19 00:39] wakes up
[1518-10-11 00:29] falls asleep
[1518-05-09 00:22] wakes up
[1518-11-01 00:57] wakes up
[1518-03-17 00:02] falls asleep
[1518-08-29 00:43] falls asleep
[1518-06-07 00:06] wakes up
[1518-07-02 00:53] falls asleep
[1518-08-06 00:54] falls asleep
[1518-10-25 00:50] falls asleep
[1518-03-13 00:42] wakes up
[1518-04-15 23:57] Guard #2953 begins shift
[1518-05-21 23:50] Guard #971 begins shift
[1518-08-26 00:04] falls asleep
[1518-09-18 00:01] Guard #1877 begins shift
[1518-04-13 23:57] Guard #683 begins shift
[1518-06-27 23:59] Guard #2953 begins shift
[1518-04-25 23:56] Guard #653 begins shift
[1518-05-28 00:07] falls asleep
[1518-06-14 00:56] wakes up
[1518-09-30 00:28] falls asleep
[1518-05-05 00:26] wakes up
[1518-05-24 00:54] falls asleep
[1518-11-16 00:05] falls asleep
[1518-07-15 00:55] wakes up
[1518-11-04 00:02] Guard #3319 begins shift
[1518-04-24 00:38] wakes up
[1518-08-08 00:39] wakes up
[1518-08-09 00:54] wakes up
[1518-10-14 00:31] falls asleep
[1518-07-12 00:44] falls asleep
[1518-10-26 00:03] Guard #2953 begins shift
[1518-05-03 23:56] Guard #641 begins shift
[1518-09-03 00:04] Guard #1069 begins shift
[1518-06-08 00:36] wakes up
[1518-11-22 00:58] wakes up
[1518-04-06 00:18] falls asleep
[1518-04-12 00:53] wakes up
[1518-04-04 00:52] wakes up
[1518-06-29 00:58] wakes up
[1518-04-19 00:36] wakes up
[1518-03-18 00:00] falls asleep
[1518-08-16 00:02] Guard #641 begins shift
[1518-09-01 00:52] wakes up
[1518-05-19 00:01] falls asleep
[1518-09-22 00:45] falls asleep
[1518-03-07 00:24] falls asleep
[1518-03-13 00:22] falls asleep
[1518-04-17 00:01] Guard #971 begins shift
[1518-04-08 00:53] wakes up
[1518-03-17 00:58] wakes up
[1518-06-11 00:16] falls asleep
[1518-06-07 23:54] Guard #3079 begins shift
[1518-11-10 23:56] Guard #3109 begins shift
[1518-07-21 00:53] falls asleep
[1518-05-20 00:52] wakes up
[1518-09-09 00:25] falls asleep
[1518-08-10 00:43] wakes up
[1518-04-17 23:59] Guard #1117 begins shift
[1518-08-07 00:55] falls asleep
[1518-05-24 00:02] Guard #3109 begins shift
[1518-08-18 00:46] wakes up
[1518-05-03 00:59] wakes up
[1518-05-20 00:50] falls asleep
[1518-10-13 00:02] falls asleep
[1518-06-02 00:43] falls asleep
[1518-05-30 00:27] falls asleep
[1518-04-19 23:57] Guard #653 begins shift
[1518-10-18 00:01] Guard #971 begins shift
[1518-04-03 00:03] Guard #3301 begins shift
[1518-11-01 00:17] falls asleep
[1518-07-27 00:49] wakes up
[1518-08-01 00:38] falls asleep
[1518-07-23 00:50] wakes up
[1518-11-10 00:44] wakes up
[1518-08-20 00:46] wakes up
[1518-10-21 00:40] falls asleep
[1518-04-09 00:48] wakes up
[1518-07-30 00:33] falls asleep
[1518-09-28 00:07] falls asleep
[1518-04-13 00:00] Guard #919 begins shift
[1518-08-19 00:57] wakes up
[1518-05-25 00:04] Guard #683 begins shift
[1518-09-14 00:04] Guard #3109 begins shift
[1518-06-28 00:58] wakes up
[1518-10-12 00:01] Guard #683 begins shift
[1518-08-11 00:34] wakes up
[1518-06-20 00:24] falls asleep
[1518-11-16 00:54] wakes up
[1518-08-02 00:43] wakes up
[1518-10-31 00:50] wakes up
[1518-03-16 00:50] wakes up
[1518-10-06 00:54] wakes up
[1518-07-17 00:20] falls asleep
[1518-03-19 00:29] wakes up
[1518-10-30 00:22] falls asleep
[1518-11-15 00:48] wakes up
[1518-08-15 00:12] falls asleep
[1518-10-06 00:41] falls asleep
[1518-05-01 00:04] falls asleep
[1518-03-31 00:15] falls asleep
[1518-08-09 23:58] Guard #1931 begins shift
[1518-06-04 00:30] falls asleep
[1518-05-22 00:08] wakes up
[1518-07-20 00:02] Guard #1877 begins shift
[1518-04-12 00:17] wakes up
[1518-08-31 00:26] wakes up
[1518-04-17 00:39] falls asleep
[1518-10-02 00:35] wakes up
[1518-07-23 00:11] falls asleep
[1518-09-26 23:58] Guard #2179 begins shift
[1518-06-06 00:39] falls asleep
[1518-08-25 00:22] wakes up
[1518-07-04 23:53] Guard #3109 begins shift
[1518-09-01 00:57] falls asleep
[1518-07-01 00:04] Guard #3301 begins shift
[1518-09-03 00:41] wakes up
[1518-09-09 23:57] Guard #971 begins shift
[1518-03-08 00:59] wakes up
[1518-09-11 00:58] wakes up
[1518-04-24 00:16] falls asleep
[1518-10-19 00:49] wakes up
[1518-10-23 00:55] falls asleep
[1518-10-27 00:10] wakes up
[1518-10-15 23:58] Guard #2953 begins shift
[1518-10-02 00:02] Guard #919 begins shift
[1518-10-23 00:56] wakes up
[1518-07-10 00:29] falls asleep
[1518-11-19 00:11] wakes up
[1518-11-07 00:27] wakes up
[1518-07-15 00:54] falls asleep
[1518-09-26 00:26] falls asleep
[1518-05-05 00:00] Guard #1931 begins shift
[1518-05-01 00:45] wakes up
[1518-10-30 00:46] wakes up
[1518-08-01 23:53] Guard #1093 begins shift
[1518-05-04 00:40] wakes up
[1518-09-04 00:43] wakes up
[1518-05-10 00:22] wakes up
[1518-07-29 00:24] falls asleep
[1518-07-21 23:56] Guard #1877 begins shift
[1518-03-09 23:46] Guard #727 begins shift
[1518-07-12 00:25] falls asleep
[1518-06-08 00:49] falls asleep
[1518-07-23 00:03] Guard #3079 begins shift
[1518-03-19 23:56] Guard #2339 begins shift
[1518-05-14 00:58] wakes up
[1518-11-03 00:47] falls asleep
[1518-09-26 00:01] Guard #3461 begins shift
[1518-07-20 00:46] falls asleep
[1518-06-03 00:12] wakes up
[1518-11-16 00:32] falls asleep
[1518-09-22 00:06] wakes up
[1518-06-16 00:49] wakes up
[1518-10-12 00:28] falls asleep
[1518-07-27 00:00] Guard #3461 begins shift
[1518-06-20 00:38] wakes up
[1518-11-18 00:32] falls asleep
[1518-03-27 00:02] falls asleep
[1518-05-10 23:50] Guard #1117 begins shift
[1518-07-27 00:43] falls asleep
[1518-03-27 00:43] falls asleep
[1518-03-23 00:56] falls asleep
[1518-06-03 00:02] falls asleep
[1518-04-29 23:56] Guard #3319 begins shift
[1518-11-04 00:57] wakes up
[1518-06-03 00:37] falls asleep
[1518-05-09 00:41] falls asleep
[1518-11-15 23:50] Guard #1069 begins shift
[1518-09-25 00:12] falls asleep
[1518-11-05 00:01] Guard #1117 begins shift
[1518-07-27 00:27] wakes up
[1518-10-18 00:34] wakes up
[1518-11-22 00:44] wakes up
[1518-08-09 00:13] falls asleep
[1518-10-07 00:01] Guard #1877 begins shift
[1518-11-03 00:52] falls asleep
[1518-05-29 00:00] Guard #1069 begins shift
[1518-10-21 00:51] falls asleep
[1518-10-08 00:25] falls asleep
[1518-07-03 23:57] Guard #2953 begins shift
[1518-07-15 00:45] wakes up
[1518-08-11 00:52] wakes up
[1518-08-14 00:00] Guard #641 begins shift
[1518-10-11 00:23] falls asleep
[1518-10-12 00:50] wakes up
[1518-06-13 00:46] wakes up
[1518-07-16 23:58] Guard #1069 begins shift
[1518-03-17 00:40] falls asleep
[1518-07-15 00:23] falls asleep
[1518-05-09 23:48] Guard #971 begins shift
[1518-11-21 23:56] Guard #727 begins shift
[1518-07-29 00:00] Guard #1093 begins shift
[1518-06-27 00:48] wakes up
[1518-11-05 00:36] wakes up
[1518-04-25 00:02] Guard #1117 begins shift
[1518-10-24 00:04] falls asleep
[1518-03-10 00:54] wakes up
[1518-05-11 00:05] falls asleep
[1518-04-18 00:10] falls asleep
[1518-07-06 00:22] falls asleep
[1518-04-26 00:55] wakes up
[1518-03-24 23:56] Guard #683 begins shift
[1518-07-12 00:56] wakes up
[1518-08-04 00:11] falls asleep
[1518-08-23 23:59] Guard #727 begins shift
[1518-08-04 00:01] Guard #2339 begins shift
[1518-07-24 00:06] falls asleep
[1518-05-20 00:20] falls asleep
[1518-08-15 00:57] wakes up
[1518-03-29 00:23] wakes up
[1518-06-01 23:56] Guard #2339 begins shift
[1518-09-27 23:56] Guard #971 begins shift
[1518-04-08 00:01] Guard #653 begins shift
[1518-05-23 00:50] falls asleep
[1518-04-04 00:36] falls asleep
[1518-06-23 00:48] wakes up
[1518-05-12 00:41] wakes up
[1518-10-12 23:48] Guard #727 begins shift
[1518-04-12 00:08] falls asleep
[1518-07-11 00:49] wakes up
[1518-03-13 00:57] wakes up
[1518-03-06 00:23] falls asleep
[1518-03-31 00:00] Guard #1093 begins shift
[1518-09-13 00:33] wakes up
[1518-09-01 00:50] falls asleep
[1518-07-07 23:57] Guard #683 begins shift
[1518-11-14 00:04] Guard #971 begins shift
[1518-06-08 23:58] Guard #3301 begins shift
[1518-10-22 23:49] Guard #3461 begins shift
[1518-07-18 00:43] falls asleep
[1518-06-30 00:16] falls asleep
[1518-10-02 00:21] falls asleep
[1518-06-13 00:08] wakes up
[1518-05-03 00:36] falls asleep
[1518-09-23 23:58] Guard #1931 begins shift
[1518-10-26 23:46] Guard #683 begins shift
[1518-10-03 00:47] wakes up
[1518-03-23 00:48] wakes up
[1518-03-22 00:44] wakes up
[1518-09-08 00:45] falls asleep
[1518-05-17 00:02] Guard #1409 begins shift
[1518-09-16 23:59] Guard #641 begins shift
[1518-06-06 00:41] wakes up
[1518-08-29 00:00] falls asleep
[1518-10-04 00:59] wakes up
[1518-03-05 00:52] wakes up
[1518-08-16 23:59] Guard #1117 begins shift
[1518-11-03 00:55] wakes up
[1518-06-10 00:32] falls asleep
[1518-08-22 23:59] Guard #2543 begins shift
[1518-10-26 00:47] wakes up
[1518-03-11 00:29] wakes up
[1518-03-09 00:08] falls asleep
[1518-08-07 23:50] Guard #971 begins shift
[1518-05-30 23:59] Guard #3319 begins shift
[1518-03-29 00:12] falls asleep
[1518-06-06 00:00] falls asleep
[1518-09-24 00:58] wakes up
[1518-08-16 00:23] falls asleep
[1518-03-15 00:55] wakes up
[1518-09-22 00:00] falls asleep
[1518-03-07 00:33] wakes up
[1518-06-11 00:29] wakes up
[1518-03-11 00:22] falls asleep
[1518-09-05 00:01] Guard #2179 begins shift
[1518-07-22 00:50] falls asleep
[1518-05-15 00:01] Guard #3109 begins shift
[1518-11-18 00:00] Guard #1069 begins shift
[1518-10-07 00:54] wakes up
[1518-09-07 00:00] Guard #2953 begins shift
[1518-05-31 23:46] Guard #1931 begins shift
[1518-08-27 23:58] Guard #1931 begins shift
[1518-09-17 00:50] falls asleep
[1518-07-17 23:52] Guard #1877 begins shift
[1518-03-09 00:00] Guard #653 begins shift
[1518-08-21 00:19] wakes up
[1518-07-24 23:49] Guard #641 begins shift
[1518-08-05 00:08] falls asleep
[1518-03-02 00:51] falls asleep
[1518-09-07 00:13] falls asleep
[1518-06-21 23:57] Guard #919 begins shift
[1518-06-11 00:59] wakes up
[1518-09-25 00:49] wakes up
[1518-05-28 00:41] wakes up
[1518-06-14 00:03] Guard #3109 begins shift
[1518-05-29 00:41] falls asleep
[1518-09-14 00:56] falls asleep
[1518-03-16 23:46] Guard #1117 begins shift
[1518-04-23 23:59] Guard #641 begins shift
[1518-09-05 00:58] wakes up
[1518-10-09 00:38] wakes up
[1518-07-10 00:02] Guard #3109 begins shift
[1518-09-13 00:52] wakes up
[1518-10-07 00:17] falls asleep
[1518-07-19 00:57] falls asleep
[1518-09-06 00:41] falls asleep
[1518-06-26 00:45] wakes up
[1518-08-19 00:03] Guard #653 begins shift
[1518-07-11 00:01] Guard #971 begins shift
[1518-11-20 00:01] falls asleep
[1518-08-30 00:03] falls asleep
[1518-11-22 00:43] falls asleep
[1518-06-08 00:00] falls asleep
[1518-04-19 00:19] falls asleep
[1518-06-23 00:00] Guard #2543 begins shift
[1518-03-18 00:52] wakes up
[1518-06-05 00:50] wakes up
[1518-05-22 00:29] falls asleep
[1518-10-17 00:06] falls asleep
[1518-06-20 00:03] Guard #2543 begins shift
[1518-08-10 23:57] Guard #3079 begins shift
[1518-06-12 00:45] falls asleep
[1518-08-17 00:30] falls asleep
[1518-08-16 00:37] wakes up
[1518-11-14 00:42] wakes up
[1518-03-19 00:19] falls asleep
[1518-08-18 00:37] falls asleep
[1518-08-11 00:12] wakes up
[1518-04-14 00:57] wakes up
[1518-05-09 00:42] wakes up
[1518-07-31 00:51] wakes up
[1518-05-08 00:40] wakes up
[1518-05-14 00:37] falls asleep
[1518-04-16 00:54] falls asleep
[1518-04-04 00:19] falls asleep
[1518-07-20 23:58] Guard #3109 begins shift
[1518-05-21 00:40] wakes up
[1518-05-21 00:16] falls asleep
[1518-10-06 00:25] falls asleep
[1518-05-10 00:05] falls asleep
[1518-05-29 00:44] wakes up
[1518-08-18 00:55] wakes up
[1518-06-18 23:58] Guard #683 begins shift
[1518-05-18 00:02] Guard #919 begins shift
[1518-06-28 23:57] Guard #683 begins shift
[1518-05-02 00:04] Guard #1877 begins shift
[1518-07-28 00:48] wakes up
[1518-11-07 00:52] falls asleep
[1518-06-22 00:30] falls asleep
[1518-05-06 00:19] wakes up
[1518-07-07 00:00] Guard #1277 begins shift
[1518-10-07 23:57] Guard #641 begins shift
[1518-07-02 00:54] wakes up
[1518-07-02 00:00] Guard #3331 begins shift
[1518-11-20 00:55] wakes up
[1518-06-16 00:03] Guard #1877 begins shift
[1518-08-21 00:09] falls asleep
[1518-09-28 00:50] wakes up
[1518-09-01 00:59] wakes up
[1518-11-23 00:53] wakes up
[1518-03-22 00:02] Guard #971 begins shift
[1518-10-24 00:58] wakes up
[1518-03-21 00:14] falls asleep
[1518-05-21 00:29] falls asleep
[1518-03-14 00:07] wakes up
[1518-02-28 23:56] Guard #971 begins shift
[1518-09-15 00:40] falls asleep
[1518-06-12 00:37] wakes up
[1518-07-18 00:51] wakes up
[1518-08-15 00:15] wakes up
[1518-11-17 00:04] Guard #641 begins shift
[1518-03-21 00:00] Guard #1877 begins shift
[1518-07-11 00:54] wakes up
[1518-11-09 00:21] falls asleep
[1518-08-29 00:32] wakes up
[1518-11-07 00:30] falls asleep
[1518-05-26 00:52] falls asleep
[1518-08-22 00:15] falls asleep
[1518-09-12 00:46] falls asleep
[1518-04-28 00:45] wakes up
[1518-07-26 00:52] wakes up
[1518-11-09 00:14] falls asleep
[1518-10-11 00:03] Guard #653 begins shift
[1518-06-02 00:20] falls asleep
[1518-05-20 23:47] Guard #727 begins shift
[1518-06-14 00:36] falls asleep
[1518-10-28 00:45] wakes up
[1518-03-28 00:16] falls asleep
[1518-04-25 00:53] wakes up
[1518-08-13 00:38] falls asleep
[1518-03-02 00:08] falls asleep
[1518-05-22 00:02] falls asleep
[1518-06-25 00:14] falls asleep
[1518-08-05 00:00] Guard #2953 begins shift
[1518-07-24 00:46] wakes up
[1518-10-10 00:14] falls asleep
[1518-04-15 00:59] wakes up
[1518-05-29 23:59] Guard #2179 begins shift
[1518-10-17 00:04] Guard #3079 begins shift
[1518-11-13 00:36] falls asleep
[1518-05-15 00:56] wakes up
[1518-05-25 00:53] falls asleep
[1518-04-06 00:00] Guard #919 begins shift
[1518-03-22 00:39] wakes up
[1518-10-28 00:40] falls asleep
[1518-09-22 00:56] wakes up
[1518-07-19 00:10] falls asleep
[1518-10-25 00:16] falls asleep
[1518-11-05 00:22] falls asleep
[1518-07-22 00:59] wakes up
[1518-08-07 00:30] falls asleep
[1518-10-05 00:36] falls asleep
[1518-03-05 00:42] falls asleep
[1518-08-23 00:23] falls asleep
[1518-03-22 00:55] wakes up
[1518-09-01 00:42] falls asleep
[1518-06-04 00:04] Guard #2179 begins shift
[1518-11-08 00:39] wakes up
[1518-04-11 00:31] falls asleep
[1518-05-12 00:03] Guard #3331 begins shift
[1518-03-31 00:49] falls asleep
[1518-07-12 00:29] wakes up
[1518-11-08 00:02] Guard #653 begins shift
[1518-03-08 00:03] Guard #727 begins shift
[1518-04-13 00:13] falls asleep
[1518-03-05 23:59] Guard #1069 begins shift
[1518-07-24 00:55] falls asleep
[1518-07-14 00:42] wakes up
[1518-05-16 00:13] falls asleep
[1518-04-25 00:26] falls asleep
[1518-10-09 23:59] Guard #641 begins shift
[1518-03-24 00:28] falls asleep
[1518-11-07 00:04] Guard #727 begins shift
[1518-06-12 00:54] wakes up
[1518-08-21 00:32] falls asleep
[1518-05-27 00:46] wakes up
[1518-06-10 00:01] Guard #683 begins shift
[1518-10-25 00:51] wakes up
[1518-10-24 23:58] Guard #3331 begins shift
[1518-03-31 23:50] Guard #2543 begins shift
[1518-04-10 00:26] falls asleep
[1518-11-21 00:58] wakes up
[1518-03-10 00:53] falls asleep
[1518-10-04 00:37] falls asleep
[1518-03-10 00:02] falls asleep
[1518-08-20 00:00] Guard #919 begins shift
[1518-08-29 00:45] wakes up
[1518-08-15 00:02] Guard #3319 begins shift
[1518-10-16 00:10] falls asleep
[1518-08-22 00:01] Guard #971 begins shift
[1518-07-11 00:06] falls asleep
[1518-10-04 23:58] Guard #919 begins shift
[1518-03-17 00:31] wakes up
[1518-09-11 00:04] Guard #1093 begins shift
[1518-05-18 00:22] falls asleep
[1518-10-14 00:57] wakes up
[1518-05-16 00:54] wakes up
[1518-03-01 00:15] falls asleep
[1518-06-15 00:42] wakes up
[1518-10-25 00:45] wakes up
[1518-10-19 23:58] Guard #971 begins shift
[1518-06-04 00:47] wakes up
[1518-05-21 00:46] wakes up
[1518-07-29 00:53] wakes up
[1518-07-03 00:20] falls asleep
[1518-05-07 00:34] wakes up
[1518-08-01 00:00] Guard #1093 begins shift
[1518-09-02 00:56] wakes up
[1518-07-06 00:38] wakes up
[1518-04-23 00:18] falls asleep
[1518-04-30 00:11] falls asleep
[1518-03-29 23:59] Guard #3461 begins shift
[1518-07-16 00:47] wakes up
[1518-06-27 00:04] Guard #1931 begins shift
[1518-08-29 23:50] Guard #3319 begins shift
[1518-11-22 23:51] Guard #3331 begins shift
[1518-06-29 00:39] falls asleep
[1518-03-08 00:12] falls asleep
[1518-06-26 00:04] Guard #653 begins shift
[1518-09-30 00:54] wakes up
[1518-07-27 00:06] falls asleep
[1518-06-29 23:57] Guard #641 begins shift
[1518-06-08 00:33] falls asleep
[1518-08-12 00:32] falls asleep
[1518-06-03 00:56] wakes up
[1518-09-07 00:40] wakes up
[1518-05-11 00:44] wakes up
[1518-11-01 00:02] Guard #1093 begins shift
[1518-09-19 23:56] Guard #919 begins shift
[1518-03-25 00:30] falls asleep
[1518-07-20 00:16] wakes up
[1518-04-26 00:54] falls asleep
[1518-08-01 00:22] wakes up
[1518-05-21 00:00] falls asleep
[1518-04-02 00:46] wakes up
[1518-11-05 23:57] Guard #919 begins shift
[1518-10-14 00:04] Guard #3109 begins shift
[1518-05-07 00:03] falls asleep
[1518-08-17 00:47] wakes up
[1518-09-19 00:08] falls asleep
[1518-03-11 00:00] falls asleep
[1518-09-22 23:52] Guard #2179 begins shift
[1518-10-13 00:36] wakes up
[1518-08-08 00:45] wakes up
[1518-03-08 00:36] falls asleep
[1518-05-06 00:03] falls asleep
[1518-07-24 00:00] Guard #3461 begins shift
[1518-05-14 00:24] falls asleep
[1518-04-04 00:42] wakes up
[1518-09-02 00:00] Guard #653 begins shift
[1518-03-16 00:46] falls asleep
[1518-08-18 00:03] Guard #727 begins shift
[1518-03-12 00:42] wakes up
[1518-04-29 00:03] Guard #1069 begins shift
[1518-05-19 00:50] wakes up
[1518-09-27 00:13] falls asleep
[1518-11-08 00:26] falls asleep
[1518-04-09 00:02] Guard #1877 begins shift
[1518-10-23 00:01] falls asleep
[1518-08-30 00:49] wakes up
[1518-09-13 00:00] Guard #1093 begins shift
[1518-04-26 23:56] Guard #1277 begins shift
[1518-11-19 00:04] Guard #3461 begins shift
[1518-07-30 00:01] Guard #919 begins shift
[1518-04-12 00:38] falls asleep
[1518-07-19 00:34] wakes up
[1518-05-23 00:03] Guard #1877 begins shift
[1518-10-06 00:36] wakes up
[1518-09-27 00:37] wakes up
[1518-03-22 00:43] falls asleep
[1518-11-06 00:45] wakes up
[1518-06-08 00:53] wakes up
[1518-05-21 00:25] wakes up
[1518-11-23 00:00] falls asleep
[1518-09-09 00:01] Guard #1093 begins shift
[1518-07-30 00:48] wakes up
[1518-09-05 23:59] Guard #3461 begins shift
[1518-07-10 00:32] wakes up
[1518-04-05 00:36] falls asleep
[1518-10-30 00:49] falls asleep
[1518-09-21 00:18] falls asleep
[1518-04-29 00:10] falls asleep
[1518-09-08 00:57] wakes up
[1518-03-30 00:59] wakes up
[1518-06-28 00:48] falls asleep
[1518-10-31 00:48] falls asleep
[1518-03-12 00:00] Guard #3079 begins shift
[1518-04-01 23:58] Guard #971 begins shift
[1518-05-26 00:55] wakes up
[1518-09-16 00:02] Guard #919 begins shift
[1518-09-10 00:29] wakes up
[1518-07-31 00:47] falls asleep
[1518-06-12 23:58] Guard #1877 begins shift
[1518-03-24 00:07] falls asleep
[1518-07-04 00:08] falls asleep
[1518-06-18 00:57] wakes up
[1518-06-21 00:02] Guard #919 begins shift
[1518-08-21 00:00] Guard #3461 begins shift
[1518-04-04 00:32] wakes up
[1518-09-11 00:13] falls asleep
[1518-10-20 00:47] wakes up
[1518-08-12 23:58] Guard #653 begins shift
[1518-09-06 00:56] wakes up"""
