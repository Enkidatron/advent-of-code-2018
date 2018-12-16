module Day16 exposing (OpCode, Operation(..), Registers, TestCase, allOperations, boolToInt, findOpCodeNumbers, inputTextPart1, inputTextProgram, makeOpCodeOperation, opCodeIntParser, opCodeOperationParser, operationParser, part1, part2, performOperation, program, programParser, readRegister, registersParser, removePossibilities, testCaseBehavesLikeThreeOrMoreOperations, testCaseParser, testCases, testCasesParser)

import Array exposing (Array)
import Bitwise
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)


testCaseParser : Parser TestCase
testCaseParser =
    Parser.succeed TestCase
        |. Parser.symbol "Before: "
        |= registersParser
        |. Parser.spaces
        |= opCodeIntParser
        |. Parser.spaces
        |. Parser.symbol "After:"
        |. Parser.spaces
        |= registersParser


testCasesParser : Parser (List TestCase)
testCasesParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = testCaseParser
        , trailing = Parser.Optional
        }


registersParser : Parser (Array Int)
registersParser =
    Parser.succeed (\a b c d -> Array.fromList [ a, b, c, d ])
        |. Parser.symbol "["
        |= Parser.int
        |. Parser.symbol ", "
        |= Parser.int
        |. Parser.symbol ", "
        |= Parser.int
        |. Parser.symbol ", "
        |= Parser.int
        |. Parser.symbol "]"


opCodeIntParser : Parser (OpCode Int)
opCodeIntParser =
    Parser.succeed OpCode
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int


opCodeOperationParser : Parser (OpCode Operation)
opCodeOperationParser =
    Parser.succeed OpCode
        |= operationParser
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int


operationParser : Parser Operation
operationParser =
    Parser.int
        |> Parser.andThen
            (\x ->
                {- to find this mapping, I used `findOpCodeNumbers`, and then
                   analyzed the output in the repl. That function generates a
                   Dict from numbers to possible operations, but most of them
                   are compatible with multiple operations, even after all
                   test cases are considered.
                   I kept a list of the ones that I knew for sure, and used
                   the repl to filter the known operations out of the list of
                   possibilities for the other numbers until they all were known.
                -}
                case x of
                    0 ->
                        Parser.succeed MulI

                    1 ->
                        Parser.succeed BAnI

                    2 ->
                        Parser.succeed AddI

                    3 ->
                        Parser.succeed SetI

                    4 ->
                        Parser.succeed EQRR

                    5 ->
                        Parser.succeed EQIR

                    6 ->
                        Parser.succeed SetR

                    7 ->
                        Parser.succeed BOrI

                    8 ->
                        Parser.succeed GTRI

                    9 ->
                        Parser.succeed EQRI

                    10 ->
                        Parser.succeed GTIR

                    11 ->
                        Parser.succeed BOrR

                    12 ->
                        Parser.succeed AddR

                    13 ->
                        Parser.succeed GTRR

                    14 ->
                        Parser.succeed MulR

                    15 ->
                        Parser.succeed BAnR

                    _ ->
                        Parser.problem "bad op code"
            )


programParser : Parser (List (OpCode Operation))
programParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = opCodeOperationParser
        , trailing = Parser.Optional
        }


type alias TestCase =
    { before : Array Int
    , opCode : OpCode Int
    , after : Array Int
    }


type alias OpCode a =
    { op : a
    , a : Int
    , b : Int
    , c : Int
    }


testCases =
    Parser.run testCasesParser inputTextPart1
        |> Result.withDefault []


type alias Registers =
    Array Int


type Operation
    = AddR
    | AddI
    | MulR
    | MulI
    | BAnR
    | BAnI
    | BOrR
    | BOrI
    | SetR
    | SetI
    | GTIR
    | GTRI
    | GTRR
    | EQIR
    | EQRI
    | EQRR


allOperations =
    [ AddR
    , AddI
    , MulR
    , MulI
    , BAnR
    , BAnI
    , BOrR
    , BOrI
    , SetR
    , SetI
    , GTIR
    , GTRI
    , GTRR
    , EQIR
    , EQRI
    , EQRR
    ]


part1 =
    testCases
        |> List.filter testCaseBehavesLikeThreeOrMoreOperations
        |> List.length


testCaseBehavesLikeThreeOrMoreOperations : TestCase -> Bool
testCaseBehavesLikeThreeOrMoreOperations testCase =
    allOperations
        |> List.map (makeOpCodeOperation testCase.opCode)
        |> List.filter (\opCodeWithOp -> performOperation opCodeWithOp testCase.before == testCase.after)
        |> List.length
        |> (\len -> len >= 3)


makeOpCodeOperation : OpCode a -> Operation -> OpCode Operation
makeOpCodeOperation original op =
    { op = op
    , a = original.a
    , b = original.b
    , c = original.c
    }


performOperation : OpCode Operation -> Registers -> Registers
performOperation opCode registers =
    let
        newValue =
            case opCode.op of
                AddR ->
                    readRegister opCode.a registers + readRegister opCode.b registers

                AddI ->
                    readRegister opCode.a registers + opCode.b

                MulR ->
                    readRegister opCode.a registers * readRegister opCode.b registers

                MulI ->
                    readRegister opCode.a registers * opCode.b

                BAnR ->
                    Bitwise.and (readRegister opCode.a registers) (readRegister opCode.b registers)

                BAnI ->
                    Bitwise.and (readRegister opCode.a registers) opCode.b

                BOrR ->
                    Bitwise.or (readRegister opCode.a registers) (readRegister opCode.b registers)

                BOrI ->
                    Bitwise.or (readRegister opCode.a registers) opCode.b

                SetR ->
                    readRegister opCode.a registers

                SetI ->
                    opCode.a

                GTIR ->
                    (opCode.a > readRegister opCode.b registers)
                        |> boolToInt

                GTRI ->
                    (readRegister opCode.a registers > opCode.b)
                        |> boolToInt

                GTRR ->
                    (readRegister opCode.a registers > readRegister opCode.b registers)
                        |> boolToInt

                EQIR ->
                    (opCode.a == readRegister opCode.b registers)
                        |> boolToInt

                EQRI ->
                    (readRegister opCode.a registers == opCode.b)
                        |> boolToInt

                EQRR ->
                    (readRegister opCode.a registers == readRegister opCode.b registers)
                        |> boolToInt
    in
    Array.set opCode.c newValue registers


boolToInt : Bool -> Int
boolToInt bool =
    case bool of
        True ->
            1

        False ->
            0


readRegister : Int -> Registers -> Int
readRegister index registers =
    Array.get index registers |> Maybe.withDefault 0


findOpCodeNumbers =
    let
        initialPossibilities =
            List.range 0 15
                |> List.map (\x -> ( x, allOperations ))
                |> Dict.fromList
    in
    List.foldl removePossibilities initialPossibilities testCases


removePossibilities : TestCase -> Dict Int (List Operation) -> Dict Int (List Operation)
removePossibilities testCase possibilities =
    let
        ourPossibilities =
            Dict.get testCase.opCode.op possibilities
                |> Maybe.withDefault []

        newPossibilities =
            ourPossibilities
                |> List.map (makeOpCodeOperation testCase.opCode)
                |> List.filter (\opCodeWithOp -> performOperation opCodeWithOp testCase.before == testCase.after)
                |> List.map .op
    in
    Dict.insert testCase.opCode.op newPossibilities possibilities


program =
    inputTextProgram |> Parser.run programParser |> Result.withDefault []


part2 =
    List.foldl performOperation (Array.fromList [ 0, 0, 0, 0 ]) program


inputTextPart1 =
    """Before: [3, 1, 0, 1]
9 3 3 2
After:  [3, 1, 0, 1]

Before: [1, 0, 3, 1]
4 2 3 2
After:  [1, 0, 0, 1]

Before: [3, 3, 3, 3]
4 3 0 0
After:  [1, 3, 3, 3]

Before: [1, 2, 2, 2]
11 2 3 2
After:  [1, 2, 2, 2]

Before: [2, 1, 0, 0]
13 0 3 3
After:  [2, 1, 0, 1]

Before: [3, 1, 2, 3]
10 1 3 0
After:  [0, 1, 2, 3]

Before: [2, 1, 1, 1]
8 3 1 1
After:  [2, 0, 1, 1]

Before: [3, 0, 0, 3]
4 3 0 1
After:  [3, 1, 0, 3]

Before: [2, 1, 2, 0]
13 0 3 1
After:  [2, 1, 2, 0]

Before: [0, 3, 0, 0]
0 0 1 1
After:  [0, 0, 0, 0]

Before: [3, 1, 2, 2]
15 1 3 1
After:  [3, 0, 2, 2]

Before: [2, 1, 1, 1]
2 2 1 2
After:  [2, 1, 2, 1]

Before: [3, 2, 2, 3]
10 1 3 2
After:  [3, 2, 0, 3]

Before: [0, 2, 2, 3]
10 2 3 3
After:  [0, 2, 2, 0]

Before: [0, 2, 3, 1]
7 0 0 1
After:  [0, 0, 3, 1]

Before: [0, 2, 2, 1]
6 3 2 3
After:  [0, 2, 2, 1]

Before: [3, 2, 2, 1]
6 3 2 1
After:  [3, 1, 2, 1]

Before: [0, 1, 3, 0]
12 1 3 3
After:  [0, 1, 3, 1]

Before: [2, 1, 3, 1]
8 2 0 1
After:  [2, 1, 3, 1]

Before: [1, 1, 2, 3]
3 0 2 1
After:  [1, 0, 2, 3]

Before: [2, 0, 3, 0]
8 2 0 2
After:  [2, 0, 1, 0]

Before: [1, 3, 2, 2]
11 2 3 0
After:  [2, 3, 2, 2]

Before: [1, 2, 0, 1]
9 3 3 0
After:  [0, 2, 0, 1]

Before: [3, 1, 0, 3]
4 3 0 3
After:  [3, 1, 0, 1]

Before: [2, 2, 3, 3]
10 1 3 0
After:  [0, 2, 3, 3]

Before: [3, 1, 3, 3]
10 1 3 3
After:  [3, 1, 3, 0]

Before: [2, 1, 0, 3]
10 1 3 1
After:  [2, 0, 0, 3]

Before: [1, 2, 2, 1]
3 0 2 3
After:  [1, 2, 2, 0]

Before: [1, 2, 2, 3]
3 0 2 0
After:  [0, 2, 2, 3]

Before: [0, 1, 2, 1]
6 3 2 0
After:  [1, 1, 2, 1]

Before: [1, 0, 3, 0]
5 3 2 2
After:  [1, 0, 1, 0]

Before: [2, 0, 3, 2]
8 0 1 3
After:  [2, 0, 3, 1]

Before: [1, 1, 2, 2]
1 1 2 3
After:  [1, 1, 2, 0]

Before: [2, 1, 3, 0]
13 0 3 2
After:  [2, 1, 1, 0]

Before: [2, 3, 2, 2]
9 3 3 2
After:  [2, 3, 0, 2]

Before: [0, 1, 2, 2]
11 2 3 2
After:  [0, 1, 2, 2]

Before: [2, 0, 2, 0]
13 0 3 3
After:  [2, 0, 2, 1]

Before: [1, 3, 2, 1]
3 0 2 3
After:  [1, 3, 2, 0]

Before: [0, 1, 1, 2]
2 2 1 2
After:  [0, 1, 2, 2]

Before: [3, 0, 2, 2]
8 3 2 3
After:  [3, 0, 2, 0]

Before: [2, 1, 2, 2]
15 1 3 2
After:  [2, 1, 0, 2]

Before: [1, 0, 3, 2]
9 3 3 2
After:  [1, 0, 0, 2]

Before: [3, 1, 1, 1]
14 1 3 0
After:  [1, 1, 1, 1]

Before: [0, 1, 0, 2]
0 0 2 2
After:  [0, 1, 0, 2]

Before: [3, 1, 1, 2]
15 1 3 1
After:  [3, 0, 1, 2]

Before: [3, 1, 2, 1]
1 1 2 0
After:  [0, 1, 2, 1]

Before: [3, 3, 2, 2]
9 3 3 1
After:  [3, 0, 2, 2]

Before: [1, 1, 2, 0]
1 1 2 0
After:  [0, 1, 2, 0]

Before: [0, 2, 2, 2]
11 2 3 0
After:  [2, 2, 2, 2]

Before: [1, 2, 2, 0]
3 0 2 2
After:  [1, 2, 0, 0]

Before: [2, 0, 1, 3]
8 0 1 1
After:  [2, 1, 1, 3]

Before: [2, 2, 2, 0]
4 2 0 3
After:  [2, 2, 2, 1]

Before: [3, 3, 1, 1]
9 2 3 1
After:  [3, 0, 1, 1]

Before: [1, 0, 3, 0]
5 3 2 1
After:  [1, 1, 3, 0]

Before: [0, 1, 3, 2]
15 1 3 0
After:  [0, 1, 3, 2]

Before: [2, 2, 0, 3]
10 1 3 0
After:  [0, 2, 0, 3]

Before: [3, 1, 2, 2]
1 1 2 3
After:  [3, 1, 2, 0]

Before: [1, 3, 2, 2]
11 2 3 2
After:  [1, 3, 2, 2]

Before: [3, 0, 3, 0]
5 3 2 2
After:  [3, 0, 1, 0]

Before: [1, 1, 0, 0]
14 0 2 1
After:  [1, 0, 0, 0]

Before: [0, 0, 2, 1]
0 0 1 3
After:  [0, 0, 2, 0]

Before: [2, 1, 2, 0]
1 1 2 0
After:  [0, 1, 2, 0]

Before: [1, 1, 2, 3]
3 0 2 3
After:  [1, 1, 2, 0]

Before: [3, 1, 2, 2]
1 1 2 2
After:  [3, 1, 0, 2]

Before: [2, 2, 3, 2]
8 2 0 2
After:  [2, 2, 1, 2]

Before: [3, 3, 0, 0]
8 0 2 0
After:  [1, 3, 0, 0]

Before: [0, 2, 2, 2]
0 0 3 2
After:  [0, 2, 0, 2]

Before: [1, 3, 0, 1]
9 3 3 0
After:  [0, 3, 0, 1]

Before: [3, 3, 2, 2]
11 2 3 0
After:  [2, 3, 2, 2]

Before: [3, 1, 2, 2]
11 2 3 2
After:  [3, 1, 2, 2]

Before: [3, 2, 3, 0]
5 3 2 3
After:  [3, 2, 3, 1]

Before: [1, 1, 2, 1]
1 1 2 3
After:  [1, 1, 2, 0]

Before: [0, 2, 1, 3]
10 1 3 1
After:  [0, 0, 1, 3]

Before: [1, 1, 2, 1]
3 0 2 1
After:  [1, 0, 2, 1]

Before: [1, 0, 3, 0]
5 3 2 3
After:  [1, 0, 3, 1]

Before: [3, 0, 1, 3]
10 2 3 1
After:  [3, 0, 1, 3]

Before: [1, 1, 2, 3]
1 1 2 0
After:  [0, 1, 2, 3]

Before: [0, 0, 3, 1]
4 2 3 0
After:  [0, 0, 3, 1]

Before: [0, 2, 1, 1]
7 0 0 0
After:  [0, 2, 1, 1]

Before: [0, 3, 0, 1]
9 3 3 2
After:  [0, 3, 0, 1]

Before: [1, 2, 0, 1]
14 0 2 0
After:  [0, 2, 0, 1]

Before: [1, 1, 1, 1]
2 2 1 2
After:  [1, 1, 2, 1]

Before: [2, 2, 2, 0]
4 2 1 1
After:  [2, 1, 2, 0]

Before: [2, 2, 2, 2]
11 2 3 2
After:  [2, 2, 2, 2]

Before: [1, 2, 1, 3]
10 2 3 1
After:  [1, 0, 1, 3]

Before: [3, 1, 1, 0]
2 2 1 0
After:  [2, 1, 1, 0]

Before: [0, 3, 3, 0]
7 0 0 1
After:  [0, 0, 3, 0]

Before: [3, 3, 2, 2]
8 3 2 0
After:  [0, 3, 2, 2]

Before: [0, 0, 3, 0]
5 3 2 2
After:  [0, 0, 1, 0]

Before: [3, 1, 0, 2]
15 1 3 2
After:  [3, 1, 0, 2]

Before: [2, 3, 3, 1]
4 2 3 1
After:  [2, 0, 3, 1]

Before: [2, 0, 0, 0]
13 0 3 2
After:  [2, 0, 1, 0]

Before: [1, 1, 2, 2]
15 1 3 0
After:  [0, 1, 2, 2]

Before: [0, 1, 3, 3]
4 3 2 1
After:  [0, 1, 3, 3]

Before: [0, 0, 2, 3]
10 2 3 2
After:  [0, 0, 0, 3]

Before: [0, 1, 0, 3]
7 0 0 2
After:  [0, 1, 0, 3]

Before: [1, 0, 1, 3]
10 2 3 2
After:  [1, 0, 0, 3]

Before: [0, 0, 1, 1]
7 0 0 1
After:  [0, 0, 1, 1]

Before: [3, 1, 3, 0]
12 1 3 1
After:  [3, 1, 3, 0]

Before: [0, 3, 2, 1]
6 3 2 3
After:  [0, 3, 2, 1]

Before: [2, 1, 3, 0]
13 0 3 3
After:  [2, 1, 3, 1]

Before: [2, 1, 2, 3]
1 1 2 2
After:  [2, 1, 0, 3]

Before: [1, 1, 0, 0]
12 1 3 0
After:  [1, 1, 0, 0]

Before: [2, 1, 2, 2]
1 1 2 2
After:  [2, 1, 0, 2]

Before: [2, 0, 2, 0]
13 0 3 2
After:  [2, 0, 1, 0]

Before: [2, 1, 3, 1]
8 2 0 0
After:  [1, 1, 3, 1]

Before: [0, 0, 2, 2]
11 2 3 0
After:  [2, 0, 2, 2]

Before: [1, 1, 3, 2]
15 1 3 1
After:  [1, 0, 3, 2]

Before: [0, 1, 2, 2]
1 1 2 1
After:  [0, 0, 2, 2]

Before: [0, 1, 1, 2]
15 1 3 3
After:  [0, 1, 1, 0]

Before: [1, 1, 1, 0]
12 1 3 2
After:  [1, 1, 1, 0]

Before: [3, 1, 0, 1]
14 1 3 3
After:  [3, 1, 0, 1]

Before: [3, 1, 0, 1]
8 3 1 2
After:  [3, 1, 0, 1]

Before: [1, 3, 2, 1]
3 0 2 0
After:  [0, 3, 2, 1]

Before: [0, 1, 2, 1]
6 3 2 1
After:  [0, 1, 2, 1]

Before: [2, 1, 1, 0]
2 2 1 1
After:  [2, 2, 1, 0]

Before: [0, 1, 2, 1]
1 1 2 1
After:  [0, 0, 2, 1]

Before: [1, 1, 2, 2]
15 1 3 2
After:  [1, 1, 0, 2]

Before: [2, 1, 1, 0]
2 2 1 0
After:  [2, 1, 1, 0]

Before: [1, 0, 2, 1]
6 3 2 1
After:  [1, 1, 2, 1]

Before: [0, 2, 1, 0]
0 0 2 3
After:  [0, 2, 1, 0]

Before: [1, 3, 2, 3]
3 0 2 2
After:  [1, 3, 0, 3]

Before: [2, 1, 2, 2]
15 1 3 3
After:  [2, 1, 2, 0]

Before: [1, 1, 3, 1]
14 1 3 3
After:  [1, 1, 3, 1]

Before: [3, 1, 1, 2]
2 2 1 0
After:  [2, 1, 1, 2]

Before: [0, 1, 2, 3]
1 1 2 2
After:  [0, 1, 0, 3]

Before: [2, 1, 0, 0]
12 1 3 1
After:  [2, 1, 0, 0]

Before: [1, 1, 2, 0]
3 0 2 1
After:  [1, 0, 2, 0]

Before: [1, 3, 3, 0]
5 3 2 0
After:  [1, 3, 3, 0]

Before: [3, 1, 2, 1]
6 3 2 1
After:  [3, 1, 2, 1]

Before: [1, 1, 2, 2]
3 0 2 2
After:  [1, 1, 0, 2]

Before: [3, 1, 0, 0]
8 0 2 1
After:  [3, 1, 0, 0]

Before: [1, 0, 2, 2]
11 2 3 0
After:  [2, 0, 2, 2]

Before: [0, 1, 3, 2]
7 0 0 3
After:  [0, 1, 3, 0]

Before: [0, 1, 1, 1]
2 2 1 1
After:  [0, 2, 1, 1]

Before: [0, 2, 0, 1]
0 0 2 3
After:  [0, 2, 0, 0]

Before: [0, 1, 2, 2]
15 1 3 3
After:  [0, 1, 2, 0]

Before: [0, 0, 3, 2]
0 0 3 2
After:  [0, 0, 0, 2]

Before: [1, 2, 0, 3]
14 0 2 2
After:  [1, 2, 0, 3]

Before: [2, 1, 2, 0]
12 1 3 2
After:  [2, 1, 1, 0]

Before: [0, 3, 0, 2]
5 2 3 1
After:  [0, 1, 0, 2]

Before: [2, 1, 1, 1]
14 1 3 0
After:  [1, 1, 1, 1]

Before: [0, 3, 3, 1]
0 0 3 3
After:  [0, 3, 3, 0]

Before: [2, 1, 2, 1]
14 1 3 1
After:  [2, 1, 2, 1]

Before: [0, 3, 3, 0]
7 0 0 0
After:  [0, 3, 3, 0]

Before: [0, 0, 0, 2]
7 0 0 3
After:  [0, 0, 0, 0]

Before: [1, 0, 2, 1]
3 0 2 1
After:  [1, 0, 2, 1]

Before: [2, 0, 2, 3]
4 2 0 1
After:  [2, 1, 2, 3]

Before: [2, 2, 1, 1]
9 2 3 1
After:  [2, 0, 1, 1]

Before: [2, 0, 1, 0]
13 0 3 2
After:  [2, 0, 1, 0]

Before: [1, 2, 2, 3]
10 2 3 0
After:  [0, 2, 2, 3]

Before: [0, 2, 3, 3]
0 0 3 2
After:  [0, 2, 0, 3]

Before: [3, 2, 2, 1]
6 3 2 2
After:  [3, 2, 1, 1]

Before: [0, 1, 2, 1]
1 1 2 3
After:  [0, 1, 2, 0]

Before: [1, 1, 1, 0]
2 2 1 1
After:  [1, 2, 1, 0]

Before: [1, 1, 1, 3]
2 2 1 0
After:  [2, 1, 1, 3]

Before: [2, 1, 3, 0]
13 0 3 0
After:  [1, 1, 3, 0]

Before: [1, 0, 0, 3]
14 0 2 0
After:  [0, 0, 0, 3]

Before: [2, 2, 0, 0]
13 0 3 1
After:  [2, 1, 0, 0]

Before: [1, 3, 2, 1]
3 0 2 2
After:  [1, 3, 0, 1]

Before: [2, 0, 1, 3]
8 0 1 0
After:  [1, 0, 1, 3]

Before: [0, 0, 2, 1]
6 3 2 1
After:  [0, 1, 2, 1]

Before: [1, 1, 2, 1]
8 3 1 1
After:  [1, 0, 2, 1]

Before: [1, 1, 3, 2]
15 1 3 0
After:  [0, 1, 3, 2]

Before: [3, 1, 1, 3]
10 2 3 2
After:  [3, 1, 0, 3]

Before: [3, 3, 2, 1]
6 3 2 2
After:  [3, 3, 1, 1]

Before: [0, 0, 1, 3]
10 2 3 0
After:  [0, 0, 1, 3]

Before: [3, 0, 3, 0]
5 3 2 0
After:  [1, 0, 3, 0]

Before: [3, 2, 0, 3]
10 1 3 3
After:  [3, 2, 0, 0]

Before: [3, 2, 0, 1]
9 3 3 1
After:  [3, 0, 0, 1]

Before: [2, 1, 3, 3]
10 1 3 0
After:  [0, 1, 3, 3]

Before: [0, 2, 1, 1]
0 0 3 2
After:  [0, 2, 0, 1]

Before: [0, 1, 0, 0]
7 0 0 2
After:  [0, 1, 0, 0]

Before: [3, 1, 2, 0]
1 1 2 3
After:  [3, 1, 2, 0]

Before: [0, 2, 2, 1]
6 3 2 0
After:  [1, 2, 2, 1]

Before: [3, 3, 3, 1]
4 2 3 3
After:  [3, 3, 3, 0]

Before: [1, 3, 2, 2]
11 2 3 1
After:  [1, 2, 2, 2]

Before: [2, 2, 1, 0]
13 0 3 2
After:  [2, 2, 1, 0]

Before: [3, 2, 2, 1]
6 3 2 3
After:  [3, 2, 2, 1]

Before: [1, 3, 2, 2]
11 2 3 3
After:  [1, 3, 2, 2]

Before: [1, 3, 3, 0]
5 3 2 3
After:  [1, 3, 3, 1]

Before: [2, 1, 2, 0]
12 1 3 0
After:  [1, 1, 2, 0]

Before: [0, 1, 1, 2]
2 2 1 0
After:  [2, 1, 1, 2]

Before: [3, 2, 0, 3]
8 0 2 0
After:  [1, 2, 0, 3]

Before: [1, 1, 1, 3]
2 2 1 3
After:  [1, 1, 1, 2]

Before: [1, 0, 2, 1]
6 3 2 2
After:  [1, 0, 1, 1]

Before: [0, 1, 2, 2]
0 0 3 0
After:  [0, 1, 2, 2]

Before: [3, 1, 1, 0]
2 2 1 2
After:  [3, 1, 2, 0]

Before: [2, 1, 2, 1]
1 1 2 1
After:  [2, 0, 2, 1]

Before: [0, 1, 1, 1]
2 2 1 3
After:  [0, 1, 1, 2]

Before: [1, 1, 2, 3]
1 1 2 2
After:  [1, 1, 0, 3]

Before: [2, 2, 2, 0]
4 2 1 3
After:  [2, 2, 2, 1]

Before: [3, 1, 0, 0]
12 1 3 2
After:  [3, 1, 1, 0]

Before: [3, 1, 3, 2]
15 1 3 3
After:  [3, 1, 3, 0]

Before: [3, 3, 0, 2]
5 2 3 3
After:  [3, 3, 0, 1]

Before: [0, 3, 2, 0]
0 0 3 0
After:  [0, 3, 2, 0]

Before: [1, 0, 2, 0]
3 0 2 2
After:  [1, 0, 0, 0]

Before: [2, 3, 2, 2]
8 3 2 0
After:  [0, 3, 2, 2]

Before: [1, 1, 3, 2]
15 1 3 2
After:  [1, 1, 0, 2]

Before: [1, 1, 2, 0]
3 0 2 2
After:  [1, 1, 0, 0]

Before: [0, 0, 2, 2]
7 0 0 1
After:  [0, 0, 2, 2]

Before: [1, 3, 0, 1]
14 0 2 2
After:  [1, 3, 0, 1]

Before: [0, 1, 2, 3]
1 1 2 0
After:  [0, 1, 2, 3]

Before: [3, 1, 0, 0]
12 1 3 0
After:  [1, 1, 0, 0]

Before: [1, 3, 2, 3]
10 2 3 0
After:  [0, 3, 2, 3]

Before: [1, 3, 2, 0]
3 0 2 1
After:  [1, 0, 2, 0]

Before: [3, 0, 2, 2]
11 2 3 1
After:  [3, 2, 2, 2]

Before: [3, 1, 2, 2]
15 1 3 3
After:  [3, 1, 2, 0]

Before: [1, 0, 2, 3]
3 0 2 1
After:  [1, 0, 2, 3]

Before: [0, 3, 2, 2]
11 2 3 3
After:  [0, 3, 2, 2]

Before: [2, 1, 2, 2]
11 2 3 1
After:  [2, 2, 2, 2]

Before: [3, 1, 1, 3]
2 2 1 1
After:  [3, 2, 1, 3]

Before: [0, 1, 0, 1]
14 1 3 0
After:  [1, 1, 0, 1]

Before: [1, 1, 2, 1]
14 1 3 2
After:  [1, 1, 1, 1]

Before: [1, 2, 2, 2]
3 0 2 3
After:  [1, 2, 2, 0]

Before: [2, 0, 2, 2]
11 2 3 2
After:  [2, 0, 2, 2]

Before: [0, 0, 3, 0]
5 3 2 1
After:  [0, 1, 3, 0]

Before: [0, 1, 1, 2]
15 1 3 0
After:  [0, 1, 1, 2]

Before: [0, 3, 1, 2]
0 0 1 0
After:  [0, 3, 1, 2]

Before: [0, 1, 3, 1]
0 0 2 1
After:  [0, 0, 3, 1]

Before: [3, 1, 1, 0]
12 1 3 3
After:  [3, 1, 1, 1]

Before: [0, 3, 2, 1]
6 3 2 1
After:  [0, 1, 2, 1]

Before: [0, 1, 0, 0]
12 1 3 0
After:  [1, 1, 0, 0]

Before: [2, 1, 2, 3]
1 1 2 3
After:  [2, 1, 2, 0]

Before: [3, 3, 0, 1]
9 3 3 0
After:  [0, 3, 0, 1]

Before: [0, 3, 2, 2]
8 3 2 1
After:  [0, 0, 2, 2]

Before: [0, 1, 2, 3]
10 1 3 0
After:  [0, 1, 2, 3]

Before: [2, 1, 1, 0]
2 2 1 2
After:  [2, 1, 2, 0]

Before: [3, 1, 1, 1]
8 3 1 0
After:  [0, 1, 1, 1]

Before: [1, 1, 0, 1]
9 3 3 3
After:  [1, 1, 0, 0]

Before: [2, 0, 2, 1]
6 3 2 1
After:  [2, 1, 2, 1]

Before: [3, 1, 0, 1]
8 3 1 0
After:  [0, 1, 0, 1]

Before: [0, 0, 0, 1]
7 0 0 0
After:  [0, 0, 0, 1]

Before: [3, 1, 1, 3]
4 3 0 1
After:  [3, 1, 1, 3]

Before: [2, 0, 0, 2]
9 3 3 1
After:  [2, 0, 0, 2]

Before: [2, 1, 3, 2]
15 1 3 0
After:  [0, 1, 3, 2]

Before: [2, 0, 1, 1]
9 3 3 2
After:  [2, 0, 0, 1]

Before: [3, 2, 2, 1]
4 2 1 0
After:  [1, 2, 2, 1]

Before: [3, 1, 3, 2]
15 1 3 2
After:  [3, 1, 0, 2]

Before: [0, 2, 3, 0]
0 0 2 2
After:  [0, 2, 0, 0]

Before: [0, 3, 3, 0]
0 0 3 2
After:  [0, 3, 0, 0]

Before: [3, 1, 3, 1]
9 3 3 2
After:  [3, 1, 0, 1]

Before: [0, 0, 2, 0]
0 0 3 3
After:  [0, 0, 2, 0]

Before: [2, 1, 2, 0]
4 2 0 2
After:  [2, 1, 1, 0]

Before: [2, 1, 1, 2]
15 1 3 0
After:  [0, 1, 1, 2]

Before: [1, 2, 2, 2]
11 2 3 1
After:  [1, 2, 2, 2]

Before: [1, 1, 2, 3]
3 0 2 2
After:  [1, 1, 0, 3]

Before: [0, 1, 1, 0]
12 1 3 3
After:  [0, 1, 1, 1]

Before: [0, 1, 2, 2]
15 1 3 2
After:  [0, 1, 0, 2]

Before: [1, 0, 2, 3]
3 0 2 2
After:  [1, 0, 0, 3]

Before: [3, 0, 2, 1]
6 3 2 2
After:  [3, 0, 1, 1]

Before: [3, 0, 0, 2]
5 2 3 2
After:  [3, 0, 1, 2]

Before: [0, 1, 1, 1]
7 0 0 1
After:  [0, 0, 1, 1]

Before: [0, 2, 2, 2]
4 2 1 1
After:  [0, 1, 2, 2]

Before: [1, 1, 0, 2]
14 0 2 1
After:  [1, 0, 0, 2]

Before: [0, 2, 1, 1]
0 0 3 0
After:  [0, 2, 1, 1]

Before: [0, 3, 3, 3]
0 0 3 3
After:  [0, 3, 3, 0]

Before: [0, 2, 2, 2]
0 0 2 1
After:  [0, 0, 2, 2]

Before: [1, 0, 0, 0]
14 0 2 2
After:  [1, 0, 0, 0]

Before: [1, 3, 2, 0]
3 0 2 2
After:  [1, 3, 0, 0]

Before: [2, 3, 1, 0]
13 0 3 0
After:  [1, 3, 1, 0]

Before: [2, 0, 2, 0]
4 2 0 1
After:  [2, 1, 2, 0]

Before: [3, 1, 2, 0]
12 1 3 0
After:  [1, 1, 2, 0]

Before: [2, 1, 3, 1]
14 1 3 0
After:  [1, 1, 3, 1]

Before: [1, 3, 3, 3]
4 3 2 0
After:  [1, 3, 3, 3]

Before: [0, 3, 1, 3]
10 2 3 3
After:  [0, 3, 1, 0]

Before: [0, 1, 3, 1]
7 0 0 2
After:  [0, 1, 0, 1]

Before: [3, 1, 2, 0]
12 1 3 2
After:  [3, 1, 1, 0]

Before: [1, 2, 3, 3]
10 1 3 0
After:  [0, 2, 3, 3]

Before: [2, 2, 3, 3]
8 2 0 3
After:  [2, 2, 3, 1]

Before: [0, 1, 2, 0]
1 1 2 3
After:  [0, 1, 2, 0]

Before: [1, 3, 2, 3]
3 0 2 1
After:  [1, 0, 2, 3]

Before: [0, 1, 1, 0]
2 2 1 3
After:  [0, 1, 1, 2]

Before: [0, 3, 0, 2]
7 0 0 1
After:  [0, 0, 0, 2]

Before: [2, 2, 2, 0]
13 0 3 3
After:  [2, 2, 2, 1]

Before: [2, 1, 2, 0]
1 1 2 1
After:  [2, 0, 2, 0]

Before: [2, 1, 0, 1]
14 1 3 1
After:  [2, 1, 0, 1]

Before: [0, 1, 2, 0]
12 1 3 1
After:  [0, 1, 2, 0]

Before: [1, 2, 1, 3]
10 1 3 3
After:  [1, 2, 1, 0]

Before: [2, 3, 2, 3]
10 2 3 2
After:  [2, 3, 0, 3]

Before: [1, 2, 2, 2]
3 0 2 2
After:  [1, 2, 0, 2]

Before: [0, 0, 3, 1]
0 0 2 3
After:  [0, 0, 3, 0]

Before: [3, 2, 1, 3]
10 2 3 1
After:  [3, 0, 1, 3]

Before: [1, 2, 2, 2]
3 0 2 1
After:  [1, 0, 2, 2]

Before: [0, 0, 3, 0]
5 3 2 0
After:  [1, 0, 3, 0]

Before: [3, 3, 2, 2]
11 2 3 1
After:  [3, 2, 2, 2]

Before: [1, 3, 3, 1]
4 2 3 3
After:  [1, 3, 3, 0]

Before: [0, 0, 3, 1]
7 0 0 2
After:  [0, 0, 0, 1]

Before: [2, 1, 1, 0]
12 1 3 0
After:  [1, 1, 1, 0]

Before: [0, 1, 1, 0]
7 0 0 1
After:  [0, 0, 1, 0]

Before: [0, 3, 1, 1]
0 0 1 2
After:  [0, 3, 0, 1]

Before: [0, 1, 2, 1]
0 0 1 0
After:  [0, 1, 2, 1]

Before: [2, 2, 2, 0]
13 0 3 2
After:  [2, 2, 1, 0]

Before: [0, 3, 2, 2]
7 0 0 2
After:  [0, 3, 0, 2]

Before: [1, 2, 2, 1]
4 2 1 0
After:  [1, 2, 2, 1]

Before: [1, 1, 3, 1]
14 1 3 2
After:  [1, 1, 1, 1]

Before: [1, 1, 3, 1]
14 1 3 1
After:  [1, 1, 3, 1]

Before: [2, 1, 3, 2]
15 1 3 2
After:  [2, 1, 0, 2]

Before: [2, 1, 1, 2]
2 2 1 1
After:  [2, 2, 1, 2]

Before: [1, 0, 2, 2]
11 2 3 1
After:  [1, 2, 2, 2]

Before: [1, 0, 2, 0]
3 0 2 0
After:  [0, 0, 2, 0]

Before: [0, 3, 1, 1]
0 0 2 0
After:  [0, 3, 1, 1]

Before: [1, 0, 2, 3]
3 0 2 0
After:  [0, 0, 2, 3]

Before: [3, 1, 3, 2]
15 1 3 1
After:  [3, 0, 3, 2]

Before: [3, 2, 0, 2]
5 2 3 3
After:  [3, 2, 0, 1]

Before: [2, 1, 1, 3]
2 2 1 0
After:  [2, 1, 1, 3]

Before: [2, 1, 2, 0]
1 1 2 3
After:  [2, 1, 2, 0]

Before: [0, 1, 0, 2]
5 2 3 1
After:  [0, 1, 0, 2]

Before: [2, 1, 3, 2]
15 1 3 1
After:  [2, 0, 3, 2]

Before: [1, 2, 2, 0]
3 0 2 1
After:  [1, 0, 2, 0]

Before: [0, 2, 2, 0]
7 0 0 3
After:  [0, 2, 2, 0]

Before: [3, 1, 1, 0]
2 2 1 3
After:  [3, 1, 1, 2]

Before: [2, 3, 1, 0]
13 0 3 1
After:  [2, 1, 1, 0]

Before: [0, 1, 2, 2]
0 0 1 1
After:  [0, 0, 2, 2]

Before: [2, 0, 2, 2]
11 2 3 3
After:  [2, 0, 2, 2]

Before: [0, 3, 0, 1]
9 3 3 3
After:  [0, 3, 0, 0]

Before: [0, 3, 1, 2]
0 0 3 0
After:  [0, 3, 1, 2]

Before: [3, 1, 1, 2]
2 2 1 3
After:  [3, 1, 1, 2]

Before: [2, 0, 2, 2]
11 2 3 1
After:  [2, 2, 2, 2]

Before: [2, 3, 2, 0]
13 0 3 0
After:  [1, 3, 2, 0]

Before: [1, 1, 0, 2]
5 2 3 1
After:  [1, 1, 0, 2]

Before: [1, 2, 2, 1]
3 0 2 0
After:  [0, 2, 2, 1]

Before: [3, 1, 2, 2]
11 2 3 1
After:  [3, 2, 2, 2]

Before: [0, 0, 0, 3]
7 0 0 2
After:  [0, 0, 0, 3]

Before: [2, 1, 0, 0]
13 0 3 1
After:  [2, 1, 0, 0]

Before: [1, 0, 0, 0]
14 0 2 0
After:  [0, 0, 0, 0]

Before: [3, 1, 2, 3]
1 1 2 2
After:  [3, 1, 0, 3]

Before: [0, 1, 1, 2]
0 0 2 1
After:  [0, 0, 1, 2]

Before: [2, 2, 2, 3]
4 2 0 3
After:  [2, 2, 2, 1]

Before: [1, 1, 2, 2]
3 0 2 0
After:  [0, 1, 2, 2]

Before: [3, 1, 2, 1]
14 1 3 2
After:  [3, 1, 1, 1]

Before: [0, 1, 1, 1]
2 2 1 2
After:  [0, 1, 2, 1]

Before: [0, 1, 2, 2]
11 2 3 3
After:  [0, 1, 2, 2]

Before: [1, 1, 2, 0]
1 1 2 2
After:  [1, 1, 0, 0]

Before: [1, 0, 2, 1]
3 0 2 0
After:  [0, 0, 2, 1]

Before: [2, 0, 1, 0]
13 0 3 0
After:  [1, 0, 1, 0]

Before: [2, 2, 2, 3]
10 2 3 0
After:  [0, 2, 2, 3]

Before: [0, 2, 0, 2]
7 0 0 0
After:  [0, 2, 0, 2]

Before: [1, 2, 2, 3]
3 0 2 2
After:  [1, 2, 0, 3]

Before: [2, 1, 2, 3]
1 1 2 0
After:  [0, 1, 2, 3]

Before: [2, 3, 3, 1]
8 2 0 3
After:  [2, 3, 3, 1]

Before: [0, 3, 2, 2]
11 2 3 1
After:  [0, 2, 2, 2]

Before: [1, 2, 0, 3]
14 0 2 3
After:  [1, 2, 0, 0]

Before: [0, 2, 2, 1]
6 3 2 2
After:  [0, 2, 1, 1]

Before: [1, 1, 1, 1]
9 3 3 0
After:  [0, 1, 1, 1]

Before: [1, 2, 2, 2]
3 0 2 0
After:  [0, 2, 2, 2]

Before: [0, 2, 3, 0]
7 0 0 3
After:  [0, 2, 3, 0]

Before: [1, 3, 2, 2]
3 0 2 2
After:  [1, 3, 0, 2]

Before: [3, 2, 1, 3]
10 1 3 2
After:  [3, 2, 0, 3]

Before: [1, 0, 2, 3]
3 0 2 3
After:  [1, 0, 2, 0]

Before: [3, 1, 2, 1]
6 3 2 3
After:  [3, 1, 2, 1]

Before: [2, 1, 0, 2]
5 2 3 3
After:  [2, 1, 0, 1]

Before: [1, 0, 1, 2]
9 3 3 1
After:  [1, 0, 1, 2]

Before: [3, 1, 1, 3]
2 2 1 3
After:  [3, 1, 1, 2]

Before: [2, 1, 2, 2]
15 1 3 1
After:  [2, 0, 2, 2]

Before: [1, 0, 1, 1]
9 3 3 1
After:  [1, 0, 1, 1]

Before: [1, 1, 2, 2]
8 3 2 1
After:  [1, 0, 2, 2]

Before: [0, 1, 0, 0]
12 1 3 3
After:  [0, 1, 0, 1]

Before: [2, 0, 0, 2]
8 0 1 3
After:  [2, 0, 0, 1]

Before: [1, 0, 2, 2]
3 0 2 3
After:  [1, 0, 2, 0]

Before: [0, 3, 2, 2]
7 0 0 0
After:  [0, 3, 2, 2]

Before: [3, 1, 0, 3]
10 1 3 1
After:  [3, 0, 0, 3]

Before: [2, 2, 0, 0]
13 0 3 3
After:  [2, 2, 0, 1]

Before: [0, 1, 1, 1]
2 2 1 0
After:  [2, 1, 1, 1]

Before: [1, 1, 2, 0]
1 1 2 1
After:  [1, 0, 2, 0]

Before: [0, 2, 1, 3]
0 0 2 1
After:  [0, 0, 1, 3]

Before: [0, 1, 2, 2]
11 2 3 1
After:  [0, 2, 2, 2]

Before: [0, 1, 1, 3]
2 2 1 2
After:  [0, 1, 2, 3]

Before: [0, 2, 3, 1]
4 2 3 2
After:  [0, 2, 0, 1]

Before: [2, 3, 3, 0]
5 3 2 3
After:  [2, 3, 3, 1]

Before: [3, 1, 2, 2]
1 1 2 1
After:  [3, 0, 2, 2]

Before: [2, 2, 2, 1]
6 3 2 0
After:  [1, 2, 2, 1]

Before: [0, 1, 1, 3]
2 2 1 0
After:  [2, 1, 1, 3]

Before: [3, 1, 3, 0]
5 3 2 2
After:  [3, 1, 1, 0]

Before: [3, 1, 2, 1]
6 3 2 2
After:  [3, 1, 1, 1]

Before: [1, 1, 0, 3]
14 0 2 3
After:  [1, 1, 0, 0]

Before: [2, 1, 1, 2]
15 1 3 1
After:  [2, 0, 1, 2]

Before: [3, 0, 3, 0]
5 3 2 1
After:  [3, 1, 3, 0]

Before: [2, 0, 2, 2]
8 0 1 3
After:  [2, 0, 2, 1]

Before: [0, 0, 2, 2]
11 2 3 1
After:  [0, 2, 2, 2]

Before: [1, 1, 2, 1]
3 0 2 0
After:  [0, 1, 2, 1]

Before: [3, 1, 2, 2]
1 1 2 0
After:  [0, 1, 2, 2]

Before: [1, 0, 2, 3]
10 2 3 0
After:  [0, 0, 2, 3]

Before: [3, 1, 1, 1]
2 2 1 1
After:  [3, 2, 1, 1]

Before: [3, 1, 0, 2]
15 1 3 0
After:  [0, 1, 0, 2]

Before: [1, 3, 1, 1]
9 2 3 0
After:  [0, 3, 1, 1]

Before: [3, 1, 2, 2]
11 2 3 3
After:  [3, 1, 2, 2]

Before: [0, 3, 0, 1]
7 0 0 2
After:  [0, 3, 0, 1]

Before: [1, 2, 0, 2]
14 0 2 2
After:  [1, 2, 0, 2]

Before: [1, 3, 2, 1]
6 3 2 2
After:  [1, 3, 1, 1]

Before: [2, 3, 3, 3]
8 2 0 1
After:  [2, 1, 3, 3]

Before: [1, 2, 2, 3]
3 0 2 3
After:  [1, 2, 2, 0]

Before: [0, 1, 3, 0]
12 1 3 0
After:  [1, 1, 3, 0]

Before: [3, 1, 0, 2]
5 2 3 3
After:  [3, 1, 0, 1]

Before: [0, 2, 2, 0]
4 2 1 1
After:  [0, 1, 2, 0]

Before: [2, 2, 1, 0]
13 0 3 3
After:  [2, 2, 1, 1]

Before: [0, 1, 0, 1]
9 3 3 2
After:  [0, 1, 0, 1]

Before: [0, 1, 2, 1]
6 3 2 3
After:  [0, 1, 2, 1]

Before: [1, 3, 2, 0]
3 0 2 0
After:  [0, 3, 2, 0]

Before: [0, 1, 0, 2]
15 1 3 3
After:  [0, 1, 0, 0]

Before: [0, 3, 3, 1]
7 0 0 3
After:  [0, 3, 3, 0]

Before: [0, 1, 2, 1]
7 0 0 2
After:  [0, 1, 0, 1]

Before: [1, 2, 0, 3]
14 0 2 1
After:  [1, 0, 0, 3]

Before: [2, 1, 1, 2]
2 2 1 2
After:  [2, 1, 2, 2]

Before: [1, 0, 2, 2]
11 2 3 3
After:  [1, 0, 2, 2]

Before: [3, 1, 1, 2]
2 2 1 1
After:  [3, 2, 1, 2]

Before: [2, 1, 1, 1]
2 2 1 0
After:  [2, 1, 1, 1]

Before: [0, 0, 3, 0]
0 0 2 1
After:  [0, 0, 3, 0]

Before: [3, 1, 2, 1]
1 1 2 2
After:  [3, 1, 0, 1]

Before: [2, 2, 2, 1]
6 3 2 3
After:  [2, 2, 2, 1]

Before: [0, 1, 3, 1]
4 2 3 0
After:  [0, 1, 3, 1]

Before: [2, 2, 3, 3]
8 2 0 1
After:  [2, 1, 3, 3]

Before: [0, 3, 0, 2]
7 0 0 0
After:  [0, 3, 0, 2]

Before: [1, 1, 3, 0]
5 3 2 1
After:  [1, 1, 3, 0]

Before: [3, 1, 1, 3]
10 2 3 1
After:  [3, 0, 1, 3]

Before: [0, 1, 2, 1]
1 1 2 2
After:  [0, 1, 0, 1]

Before: [3, 2, 3, 3]
4 3 0 2
After:  [3, 2, 1, 3]

Before: [1, 2, 2, 2]
11 2 3 3
After:  [1, 2, 2, 2]

Before: [1, 1, 2, 2]
11 2 3 1
After:  [1, 2, 2, 2]

Before: [1, 1, 1, 2]
15 1 3 3
After:  [1, 1, 1, 0]

Before: [0, 1, 3, 0]
5 3 2 3
After:  [0, 1, 3, 1]

Before: [2, 1, 3, 1]
4 2 3 1
After:  [2, 0, 3, 1]

Before: [3, 0, 2, 2]
11 2 3 3
After:  [3, 0, 2, 2]

Before: [0, 3, 0, 3]
7 0 0 3
After:  [0, 3, 0, 0]

Before: [0, 0, 2, 3]
7 0 0 3
After:  [0, 0, 2, 0]

Before: [0, 1, 3, 2]
9 3 3 3
After:  [0, 1, 3, 0]

Before: [2, 1, 1, 3]
2 2 1 2
After:  [2, 1, 2, 3]

Before: [2, 1, 2, 2]
4 2 0 1
After:  [2, 1, 2, 2]

Before: [3, 1, 3, 0]
5 3 2 0
After:  [1, 1, 3, 0]

Before: [1, 0, 0, 2]
5 2 3 2
After:  [1, 0, 1, 2]

Before: [2, 0, 3, 0]
13 0 3 0
After:  [1, 0, 3, 0]

Before: [2, 1, 1, 1]
8 3 1 2
After:  [2, 1, 0, 1]

Before: [0, 1, 1, 2]
15 1 3 2
After:  [0, 1, 0, 2]

Before: [1, 3, 0, 3]
14 0 2 3
After:  [1, 3, 0, 0]

Before: [0, 1, 2, 3]
0 0 1 1
After:  [0, 0, 2, 3]

Before: [2, 1, 2, 2]
1 1 2 1
After:  [2, 0, 2, 2]

Before: [2, 2, 3, 0]
8 2 0 3
After:  [2, 2, 3, 1]

Before: [1, 1, 2, 2]
1 1 2 1
After:  [1, 0, 2, 2]

Before: [0, 1, 3, 2]
15 1 3 2
After:  [0, 1, 0, 2]

Before: [3, 1, 0, 2]
15 1 3 3
After:  [3, 1, 0, 0]

Before: [1, 0, 1, 3]
10 2 3 1
After:  [1, 0, 1, 3]

Before: [3, 2, 0, 0]
8 0 2 0
After:  [1, 2, 0, 0]

Before: [1, 0, 3, 0]
5 3 2 0
After:  [1, 0, 3, 0]

Before: [0, 0, 0, 3]
7 0 0 0
After:  [0, 0, 0, 3]

Before: [1, 3, 3, 1]
4 2 3 0
After:  [0, 3, 3, 1]

Before: [2, 1, 0, 1]
8 3 1 2
After:  [2, 1, 0, 1]

Before: [2, 0, 2, 2]
9 3 3 1
After:  [2, 0, 2, 2]

Before: [2, 1, 2, 0]
13 0 3 3
After:  [2, 1, 2, 1]

Before: [1, 1, 0, 2]
15 1 3 2
After:  [1, 1, 0, 2]

Before: [3, 1, 2, 2]
11 2 3 0
After:  [2, 1, 2, 2]

Before: [2, 0, 0, 0]
13 0 3 3
After:  [2, 0, 0, 1]

Before: [3, 2, 3, 1]
9 3 3 3
After:  [3, 2, 3, 0]

Before: [1, 0, 2, 2]
11 2 3 2
After:  [1, 0, 2, 2]

Before: [3, 1, 1, 0]
8 2 1 0
After:  [0, 1, 1, 0]

Before: [0, 3, 3, 1]
9 3 3 2
After:  [0, 3, 0, 1]

Before: [2, 0, 1, 0]
13 0 3 1
After:  [2, 1, 1, 0]

Before: [2, 2, 0, 0]
13 0 3 0
After:  [1, 2, 0, 0]

Before: [1, 3, 2, 2]
3 0 2 1
After:  [1, 0, 2, 2]

Before: [3, 2, 0, 3]
10 1 3 2
After:  [3, 2, 0, 3]

Before: [3, 1, 2, 2]
15 1 3 0
After:  [0, 1, 2, 2]

Before: [3, 1, 1, 3]
8 2 1 0
After:  [0, 1, 1, 3]

Before: [0, 2, 3, 0]
5 3 2 3
After:  [0, 2, 3, 1]

Before: [0, 3, 3, 3]
4 3 2 3
After:  [0, 3, 3, 1]

Before: [1, 3, 3, 1]
9 3 3 3
After:  [1, 3, 3, 0]

Before: [0, 3, 2, 2]
11 2 3 2
After:  [0, 3, 2, 2]

Before: [1, 2, 2, 2]
11 2 3 0
After:  [2, 2, 2, 2]

Before: [2, 1, 2, 1]
8 3 1 3
After:  [2, 1, 2, 0]

Before: [0, 0, 2, 2]
11 2 3 3
After:  [0, 0, 2, 2]

Before: [0, 1, 0, 2]
15 1 3 2
After:  [0, 1, 0, 2]

Before: [0, 3, 2, 3]
0 0 2 2
After:  [0, 3, 0, 3]

Before: [0, 1, 3, 3]
0 0 1 2
After:  [0, 1, 0, 3]

Before: [2, 2, 1, 0]
13 0 3 0
After:  [1, 2, 1, 0]

Before: [3, 1, 1, 2]
15 1 3 0
After:  [0, 1, 1, 2]

Before: [2, 3, 2, 1]
6 3 2 3
After:  [2, 3, 2, 1]

Before: [1, 0, 0, 3]
14 0 2 3
After:  [1, 0, 0, 0]

Before: [1, 2, 0, 2]
5 2 3 0
After:  [1, 2, 0, 2]

Before: [3, 0, 2, 3]
4 3 0 2
After:  [3, 0, 1, 3]

Before: [1, 3, 2, 0]
3 0 2 3
After:  [1, 3, 2, 0]

Before: [3, 1, 2, 0]
1 1 2 0
After:  [0, 1, 2, 0]

Before: [3, 1, 2, 0]
12 1 3 3
After:  [3, 1, 2, 1]

Before: [1, 3, 2, 1]
6 3 2 1
After:  [1, 1, 2, 1]

Before: [0, 0, 2, 3]
10 2 3 1
After:  [0, 0, 2, 3]

Before: [1, 2, 0, 0]
14 0 2 0
After:  [0, 2, 0, 0]

Before: [2, 0, 1, 0]
13 0 3 3
After:  [2, 0, 1, 1]

Before: [0, 2, 2, 2]
11 2 3 2
After:  [0, 2, 2, 2]

Before: [2, 0, 2, 1]
6 3 2 0
After:  [1, 0, 2, 1]

Before: [1, 1, 2, 2]
11 2 3 3
After:  [1, 1, 2, 2]

Before: [0, 3, 0, 2]
0 0 1 3
After:  [0, 3, 0, 0]

Before: [0, 0, 2, 2]
7 0 0 0
After:  [0, 0, 2, 2]

Before: [1, 0, 2, 1]
6 3 2 3
After:  [1, 0, 2, 1]

Before: [2, 3, 2, 3]
4 2 0 0
After:  [1, 3, 2, 3]

Before: [1, 1, 2, 2]
15 1 3 3
After:  [1, 1, 2, 0]

Before: [2, 0, 1, 3]
10 2 3 3
After:  [2, 0, 1, 0]

Before: [0, 2, 2, 1]
0 0 2 3
After:  [0, 2, 2, 0]

Before: [2, 3, 3, 3]
4 3 2 3
After:  [2, 3, 3, 1]

Before: [1, 2, 2, 3]
10 2 3 1
After:  [1, 0, 2, 3]

Before: [0, 2, 3, 0]
7 0 0 0
After:  [0, 2, 3, 0]

Before: [1, 0, 0, 3]
14 0 2 2
After:  [1, 0, 0, 3]

Before: [3, 3, 2, 2]
8 3 2 2
After:  [3, 3, 0, 2]

Before: [0, 2, 2, 2]
11 2 3 3
After:  [0, 2, 2, 2]

Before: [3, 1, 3, 0]
12 1 3 0
After:  [1, 1, 3, 0]

Before: [3, 3, 0, 3]
8 0 2 3
After:  [3, 3, 0, 1]

Before: [2, 1, 0, 0]
13 0 3 2
After:  [2, 1, 1, 0]

Before: [2, 1, 1, 2]
2 2 1 3
After:  [2, 1, 1, 2]

Before: [2, 1, 3, 1]
8 3 1 1
After:  [2, 0, 3, 1]

Before: [0, 3, 1, 3]
0 0 1 2
After:  [0, 3, 0, 3]

Before: [2, 0, 3, 0]
13 0 3 3
After:  [2, 0, 3, 1]

Before: [3, 3, 0, 2]
5 2 3 2
After:  [3, 3, 1, 2]

Before: [1, 0, 2, 0]
3 0 2 3
After:  [1, 0, 2, 0]

Before: [1, 1, 2, 1]
6 3 2 3
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 1]
6 3 2 1
After:  [2, 1, 2, 1]

Before: [3, 1, 1, 1]
8 2 1 0
After:  [0, 1, 1, 1]

Before: [1, 0, 2, 2]
3 0 2 0
After:  [0, 0, 2, 2]

Before: [3, 1, 2, 3]
1 1 2 0
After:  [0, 1, 2, 3]

Before: [1, 1, 2, 1]
6 3 2 1
After:  [1, 1, 2, 1]

Before: [0, 2, 1, 3]
10 1 3 2
After:  [0, 2, 0, 3]

Before: [2, 1, 2, 2]
1 1 2 3
After:  [2, 1, 2, 0]

Before: [0, 0, 0, 0]
7 0 0 3
After:  [0, 0, 0, 0]

Before: [2, 3, 0, 0]
13 0 3 3
After:  [2, 3, 0, 1]

Before: [0, 0, 2, 3]
10 2 3 0
After:  [0, 0, 2, 3]

Before: [0, 3, 2, 3]
0 0 2 1
After:  [0, 0, 2, 3]

Before: [0, 2, 3, 3]
7 0 0 0
After:  [0, 2, 3, 3]

Before: [1, 0, 1, 3]
10 2 3 3
After:  [1, 0, 1, 0]

Before: [0, 1, 1, 0]
12 1 3 1
After:  [0, 1, 1, 0]

Before: [1, 1, 1, 1]
9 2 3 3
After:  [1, 1, 1, 0]

Before: [1, 2, 0, 1]
14 0 2 3
After:  [1, 2, 0, 0]

Before: [0, 1, 2, 2]
8 3 2 3
After:  [0, 1, 2, 0]

Before: [1, 1, 1, 2]
15 1 3 2
After:  [1, 1, 0, 2]

Before: [0, 2, 2, 2]
0 0 3 3
After:  [0, 2, 2, 0]

Before: [0, 1, 1, 1]
14 1 3 0
After:  [1, 1, 1, 1]

Before: [1, 3, 0, 2]
5 2 3 2
After:  [1, 3, 1, 2]

Before: [0, 3, 0, 1]
7 0 0 3
After:  [0, 3, 0, 0]

Before: [2, 1, 0, 1]
14 1 3 0
After:  [1, 1, 0, 1]

Before: [1, 0, 2, 0]
3 0 2 1
After:  [1, 0, 2, 0]

Before: [2, 0, 0, 2]
5 2 3 2
After:  [2, 0, 1, 2]

Before: [3, 1, 1, 2]
9 3 3 3
After:  [3, 1, 1, 0]

Before: [2, 1, 2, 2]
11 2 3 0
After:  [2, 1, 2, 2]

Before: [0, 3, 0, 3]
0 0 2 3
After:  [0, 3, 0, 0]

Before: [1, 1, 2, 1]
3 0 2 2
After:  [1, 1, 0, 1]

Before: [0, 1, 1, 0]
12 1 3 0
After:  [1, 1, 1, 0]

Before: [3, 1, 1, 1]
8 3 1 2
After:  [3, 1, 0, 1]

Before: [2, 0, 2, 0]
13 0 3 1
After:  [2, 1, 2, 0]

Before: [3, 0, 3, 2]
9 3 3 1
After:  [3, 0, 3, 2]

Before: [1, 0, 0, 0]
14 0 2 3
After:  [1, 0, 0, 0]

Before: [3, 1, 3, 1]
4 2 3 3
After:  [3, 1, 3, 0]

Before: [1, 1, 1, 0]
2 2 1 0
After:  [2, 1, 1, 0]

Before: [2, 1, 1, 0]
12 1 3 1
After:  [2, 1, 1, 0]

Before: [2, 2, 2, 3]
4 2 0 2
After:  [2, 2, 1, 3]

Before: [1, 1, 1, 1]
2 2 1 3
After:  [1, 1, 1, 2]

Before: [2, 1, 2, 1]
1 1 2 0
After:  [0, 1, 2, 1]

Before: [0, 1, 2, 3]
1 1 2 3
After:  [0, 1, 2, 0]

Before: [3, 1, 1, 3]
4 3 0 0
After:  [1, 1, 1, 3]

Before: [3, 0, 2, 1]
9 3 3 2
After:  [3, 0, 0, 1]

Before: [3, 1, 0, 2]
8 0 2 0
After:  [1, 1, 0, 2]

Before: [0, 2, 2, 1]
7 0 0 0
After:  [0, 2, 2, 1]

Before: [0, 1, 3, 1]
14 1 3 0
After:  [1, 1, 3, 1]

Before: [0, 1, 2, 2]
1 1 2 3
After:  [0, 1, 2, 0]

Before: [2, 1, 0, 1]
8 3 1 1
After:  [2, 0, 0, 1]

Before: [2, 3, 3, 0]
13 0 3 1
After:  [2, 1, 3, 0]

Before: [3, 1, 2, 0]
12 1 3 1
After:  [3, 1, 2, 0]

Before: [0, 3, 2, 1]
7 0 0 1
After:  [0, 0, 2, 1]

Before: [2, 3, 2, 2]
11 2 3 0
After:  [2, 3, 2, 2]

Before: [0, 1, 1, 3]
2 2 1 1
After:  [0, 2, 1, 3]

Before: [0, 2, 2, 3]
10 2 3 1
After:  [0, 0, 2, 3]

Before: [2, 3, 3, 1]
9 3 3 3
After:  [2, 3, 3, 0]

Before: [2, 1, 0, 2]
15 1 3 0
After:  [0, 1, 0, 2]

Before: [0, 0, 2, 1]
6 3 2 3
After:  [0, 0, 2, 1]

Before: [2, 1, 0, 2]
15 1 3 2
After:  [2, 1, 0, 2]

Before: [1, 1, 1, 3]
2 2 1 1
After:  [1, 2, 1, 3]

Before: [0, 0, 1, 0]
7 0 0 2
After:  [0, 0, 0, 0]

Before: [3, 1, 3, 2]
15 1 3 0
After:  [0, 1, 3, 2]

Before: [1, 1, 3, 0]
12 1 3 0
After:  [1, 1, 3, 0]

Before: [0, 1, 3, 3]
7 0 0 1
After:  [0, 0, 3, 3]

Before: [1, 1, 3, 3]
10 1 3 1
After:  [1, 0, 3, 3]

Before: [0, 2, 3, 3]
10 1 3 3
After:  [0, 2, 3, 0]

Before: [1, 1, 1, 1]
8 2 1 3
After:  [1, 1, 1, 0]

Before: [1, 1, 1, 0]
2 2 1 3
After:  [1, 1, 1, 2]

Before: [2, 3, 3, 0]
13 0 3 3
After:  [2, 3, 3, 1]

Before: [3, 1, 0, 0]
12 1 3 3
After:  [3, 1, 0, 1]

Before: [1, 3, 0, 2]
5 2 3 1
After:  [1, 1, 0, 2]

Before: [2, 2, 3, 3]
10 1 3 3
After:  [2, 2, 3, 0]

Before: [2, 1, 2, 1]
6 3 2 0
After:  [1, 1, 2, 1]

Before: [0, 1, 3, 0]
12 1 3 1
After:  [0, 1, 3, 0]

Before: [3, 2, 2, 3]
10 2 3 2
After:  [3, 2, 0, 3]

Before: [0, 3, 1, 3]
10 2 3 1
After:  [0, 0, 1, 3]

Before: [3, 0, 0, 1]
9 3 3 0
After:  [0, 0, 0, 1]

Before: [2, 1, 0, 0]
13 0 3 0
After:  [1, 1, 0, 0]

Before: [3, 0, 2, 2]
11 2 3 0
After:  [2, 0, 2, 2]

Before: [3, 0, 0, 3]
8 0 2 2
After:  [3, 0, 1, 3]

Before: [0, 0, 0, 2]
7 0 0 0
After:  [0, 0, 0, 2]

Before: [0, 0, 2, 1]
6 3 2 0
After:  [1, 0, 2, 1]

Before: [1, 1, 1, 2]
2 2 1 3
After:  [1, 1, 1, 2]

Before: [2, 3, 3, 1]
4 2 3 3
After:  [2, 3, 3, 0]

Before: [0, 1, 1, 2]
15 1 3 1
After:  [0, 0, 1, 2]

Before: [0, 3, 3, 0]
0 0 2 0
After:  [0, 3, 3, 0]

Before: [2, 2, 3, 3]
4 3 2 1
After:  [2, 1, 3, 3]

Before: [1, 2, 2, 0]
3 0 2 0
After:  [0, 2, 2, 0]

Before: [1, 0, 2, 1]
3 0 2 2
After:  [1, 0, 0, 1]

Before: [0, 1, 3, 0]
5 3 2 1
After:  [0, 1, 3, 0]

Before: [0, 2, 2, 3]
10 2 3 0
After:  [0, 2, 2, 3]

Before: [2, 3, 1, 1]
9 2 3 2
After:  [2, 3, 0, 1]

Before: [3, 2, 0, 3]
10 1 3 0
After:  [0, 2, 0, 3]

Before: [0, 0, 1, 3]
7 0 0 1
After:  [0, 0, 1, 3]

Before: [3, 1, 2, 1]
6 3 2 0
After:  [1, 1, 2, 1]

Before: [1, 0, 3, 1]
4 2 3 3
After:  [1, 0, 3, 0]

Before: [0, 1, 2, 0]
1 1 2 1
After:  [0, 0, 2, 0]

Before: [2, 3, 2, 1]
6 3 2 1
After:  [2, 1, 2, 1]

Before: [3, 1, 1, 1]
2 2 1 2
After:  [3, 1, 2, 1]

Before: [3, 1, 2, 3]
1 1 2 3
After:  [3, 1, 2, 0]

Before: [3, 0, 2, 3]
4 3 0 1
After:  [3, 1, 2, 3]

Before: [1, 2, 2, 1]
3 0 2 1
After:  [1, 0, 2, 1]

Before: [1, 2, 2, 1]
6 3 2 2
After:  [1, 2, 1, 1]

Before: [0, 1, 2, 0]
1 1 2 2
After:  [0, 1, 0, 0]

Before: [3, 1, 1, 1]
9 3 3 2
After:  [3, 1, 0, 1]

Before: [1, 2, 0, 2]
14 0 2 3
After:  [1, 2, 0, 0]

Before: [2, 3, 2, 2]
11 2 3 2
After:  [2, 3, 2, 2]

Before: [1, 0, 0, 2]
14 0 2 3
After:  [1, 0, 0, 0]

Before: [3, 3, 1, 2]
9 3 3 0
After:  [0, 3, 1, 2]

Before: [2, 2, 2, 2]
4 2 1 2
After:  [2, 2, 1, 2]

Before: [1, 1, 2, 1]
3 0 2 3
After:  [1, 1, 2, 0]

Before: [2, 3, 3, 1]
9 3 3 2
After:  [2, 3, 0, 1]

Before: [2, 1, 2, 0]
13 0 3 0
After:  [1, 1, 2, 0]

Before: [0, 1, 3, 3]
10 1 3 3
After:  [0, 1, 3, 0]

Before: [2, 2, 2, 2]
11 2 3 1
After:  [2, 2, 2, 2]

Before: [3, 2, 0, 0]
8 0 2 3
After:  [3, 2, 0, 1]

Before: [2, 3, 3, 0]
5 3 2 2
After:  [2, 3, 1, 0]

Before: [2, 0, 2, 0]
13 0 3 0
After:  [1, 0, 2, 0]

Before: [0, 1, 0, 2]
7 0 0 0
After:  [0, 1, 0, 2]

Before: [0, 3, 0, 1]
9 3 3 0
After:  [0, 3, 0, 1]

Before: [0, 3, 2, 1]
6 3 2 0
After:  [1, 3, 2, 1]

Before: [0, 1, 0, 3]
7 0 0 1
After:  [0, 0, 0, 3]

Before: [1, 2, 0, 2]
14 0 2 1
After:  [1, 0, 0, 2]

Before: [3, 0, 1, 1]
9 3 3 2
After:  [3, 0, 0, 1]

Before: [1, 3, 2, 3]
3 0 2 3
After:  [1, 3, 2, 0]

Before: [1, 2, 3, 0]
5 3 2 1
After:  [1, 1, 3, 0]

Before: [3, 1, 0, 2]
15 1 3 1
After:  [3, 0, 0, 2]

Before: [0, 1, 2, 2]
1 1 2 0
After:  [0, 1, 2, 2]

Before: [3, 1, 1, 3]
10 2 3 3
After:  [3, 1, 1, 0]

Before: [2, 2, 3, 0]
5 3 2 3
After:  [2, 2, 3, 1]

Before: [1, 1, 2, 0]
3 0 2 0
After:  [0, 1, 2, 0]

Before: [0, 1, 1, 2]
2 2 1 3
After:  [0, 1, 1, 2]

Before: [3, 1, 2, 2]
15 1 3 2
After:  [3, 1, 0, 2]

Before: [0, 1, 0, 1]
7 0 0 1
After:  [0, 0, 0, 1]

Before: [1, 1, 1, 2]
2 2 1 0
After:  [2, 1, 1, 2]

Before: [3, 3, 0, 2]
5 2 3 0
After:  [1, 3, 0, 2]

Before: [1, 1, 2, 2]
8 3 2 0
After:  [0, 1, 2, 2]

Before: [0, 1, 2, 2]
15 1 3 0
After:  [0, 1, 2, 2]

Before: [0, 3, 2, 0]
0 0 2 2
After:  [0, 3, 0, 0]

Before: [1, 1, 2, 1]
6 3 2 2
After:  [1, 1, 1, 1]

Before: [1, 1, 1, 2]
2 2 1 1
After:  [1, 2, 1, 2]

Before: [2, 3, 2, 1]
6 3 2 2
After:  [2, 3, 1, 1]

Before: [0, 1, 1, 0]
2 2 1 2
After:  [0, 1, 2, 0]

Before: [0, 3, 2, 3]
7 0 0 2
After:  [0, 3, 0, 3]

Before: [0, 2, 2, 2]
11 2 3 1
After:  [0, 2, 2, 2]

Before: [0, 1, 3, 0]
5 3 2 2
After:  [0, 1, 1, 0]

Before: [1, 3, 0, 1]
14 0 2 0
After:  [0, 3, 0, 1]

Before: [2, 3, 2, 0]
13 0 3 3
After:  [2, 3, 2, 1]

Before: [3, 3, 3, 0]
5 3 2 1
After:  [3, 1, 3, 0]

Before: [3, 2, 2, 2]
11 2 3 0
After:  [2, 2, 2, 2]

Before: [3, 3, 2, 2]
11 2 3 2
After:  [3, 3, 2, 2]

Before: [1, 2, 3, 0]
5 3 2 3
After:  [1, 2, 3, 1]

Before: [0, 0, 2, 2]
11 2 3 2
After:  [0, 0, 2, 2]

Before: [3, 1, 1, 2]
15 1 3 3
After:  [3, 1, 1, 0]

Before: [1, 2, 2, 0]
3 0 2 3
After:  [1, 2, 2, 0]

Before: [3, 3, 2, 3]
10 2 3 3
After:  [3, 3, 2, 0]

Before: [0, 0, 3, 0]
5 3 2 3
After:  [0, 0, 3, 1]

Before: [1, 1, 2, 0]
3 0 2 3
After:  [1, 1, 2, 0]

Before: [3, 1, 2, 3]
1 1 2 1
After:  [3, 0, 2, 3]

Before: [3, 1, 2, 0]
1 1 2 2
After:  [3, 1, 0, 0]

Before: [2, 0, 2, 1]
6 3 2 3
After:  [2, 0, 2, 1]

Before: [0, 2, 2, 1]
0 0 2 0
After:  [0, 2, 2, 1]

Before: [2, 0, 2, 2]
11 2 3 0
After:  [2, 0, 2, 2]

Before: [1, 1, 0, 0]
14 0 2 0
After:  [0, 1, 0, 0]

Before: [0, 0, 1, 3]
0 0 2 2
After:  [0, 0, 0, 3]

Before: [0, 1, 3, 2]
15 1 3 3
After:  [0, 1, 3, 0]

Before: [1, 0, 2, 1]
3 0 2 3
After:  [1, 0, 2, 0]

Before: [0, 0, 0, 2]
5 2 3 1
After:  [0, 1, 0, 2]

Before: [2, 1, 3, 0]
12 1 3 2
After:  [2, 1, 1, 0]

Before: [0, 1, 2, 2]
11 2 3 0
After:  [2, 1, 2, 2]

Before: [3, 0, 2, 3]
10 2 3 2
After:  [3, 0, 0, 3]

Before: [2, 0, 1, 3]
10 2 3 2
After:  [2, 0, 0, 3]

Before: [0, 2, 1, 3]
7 0 0 3
After:  [0, 2, 1, 0]

Before: [2, 1, 3, 0]
12 1 3 3
After:  [2, 1, 3, 1]

Before: [3, 2, 2, 2]
11 2 3 2
After:  [3, 2, 2, 2]

Before: [1, 1, 0, 2]
5 2 3 0
After:  [1, 1, 0, 2]

Before: [0, 3, 1, 2]
7 0 0 3
After:  [0, 3, 1, 0]

Before: [0, 0, 3, 2]
0 0 1 1
After:  [0, 0, 3, 2]

Before: [2, 1, 2, 2]
11 2 3 3
After:  [2, 1, 2, 2]

Before: [2, 0, 0, 0]
13 0 3 1
After:  [2, 1, 0, 0]

Before: [1, 3, 2, 3]
3 0 2 0
After:  [0, 3, 2, 3]

Before: [3, 1, 1, 0]
12 1 3 1
After:  [3, 1, 1, 0]

Before: [1, 1, 3, 0]
12 1 3 3
After:  [1, 1, 3, 1]

Before: [1, 3, 2, 1]
3 0 2 1
After:  [1, 0, 2, 1]

Before: [2, 1, 1, 3]
2 2 1 3
After:  [2, 1, 1, 2]

Before: [3, 1, 1, 2]
8 2 1 3
After:  [3, 1, 1, 0]

Before: [1, 2, 0, 2]
14 0 2 0
After:  [0, 2, 0, 2]

Before: [2, 1, 1, 0]
12 1 3 2
After:  [2, 1, 1, 0]

Before: [3, 3, 0, 2]
8 0 2 3
After:  [3, 3, 0, 1]

Before: [1, 1, 3, 3]
10 1 3 0
After:  [0, 1, 3, 3]

Before: [2, 1, 3, 2]
15 1 3 3
After:  [2, 1, 3, 0]

Before: [3, 1, 1, 0]
12 1 3 0
After:  [1, 1, 1, 0]

Before: [0, 1, 2, 1]
0 0 3 2
After:  [0, 1, 0, 1]

Before: [1, 1, 1, 0]
2 2 1 2
After:  [1, 1, 2, 0]

Before: [0, 2, 1, 3]
10 1 3 3
After:  [0, 2, 1, 0]

Before: [2, 1, 3, 0]
12 1 3 1
After:  [2, 1, 3, 0]

Before: [2, 2, 3, 0]
5 3 2 2
After:  [2, 2, 1, 0]

Before: [2, 0, 2, 1]
6 3 2 2
After:  [2, 0, 1, 1]

Before: [2, 0, 2, 3]
10 2 3 2
After:  [2, 0, 0, 3]

Before: [2, 2, 1, 2]
9 3 3 3
After:  [2, 2, 1, 0]

Before: [2, 2, 3, 2]
9 3 3 1
After:  [2, 0, 3, 2]

Before: [3, 3, 2, 1]
6 3 2 0
After:  [1, 3, 2, 1]

Before: [2, 3, 2, 2]
11 2 3 3
After:  [2, 3, 2, 2]

Before: [0, 1, 3, 0]
12 1 3 2
After:  [0, 1, 1, 0]

Before: [3, 2, 2, 2]
8 3 2 1
After:  [3, 0, 2, 2]

Before: [1, 1, 2, 2]
11 2 3 2
After:  [1, 1, 2, 2]

Before: [2, 1, 2, 1]
14 1 3 0
After:  [1, 1, 2, 1]

Before: [1, 1, 2, 3]
1 1 2 3
After:  [1, 1, 2, 0]

Before: [0, 2, 0, 1]
7 0 0 3
After:  [0, 2, 0, 0]

Before: [1, 0, 2, 2]
3 0 2 2
After:  [1, 0, 0, 2]

Before: [2, 1, 3, 0]
5 3 2 1
After:  [2, 1, 3, 0]

Before: [2, 3, 2, 3]
10 2 3 0
After:  [0, 3, 2, 3]

Before: [1, 1, 3, 0]
5 3 2 0
After:  [1, 1, 3, 0]

Before: [2, 1, 1, 0]
13 0 3 1
After:  [2, 1, 1, 0]

Before: [2, 1, 2, 0]
12 1 3 1
After:  [2, 1, 2, 0]

Before: [0, 3, 2, 2]
11 2 3 0
After:  [2, 3, 2, 2]

Before: [3, 2, 1, 2]
9 3 3 3
After:  [3, 2, 1, 0]

Before: [2, 3, 2, 0]
13 0 3 2
After:  [2, 3, 1, 0]

Before: [3, 1, 3, 3]
10 1 3 0
After:  [0, 1, 3, 3]

Before: [3, 0, 2, 1]
6 3 2 1
After:  [3, 1, 2, 1]

Before: [2, 2, 3, 0]
13 0 3 2
After:  [2, 2, 1, 0]

Before: [3, 1, 3, 0]
5 3 2 1
After:  [3, 1, 3, 0]

Before: [0, 2, 2, 2]
7 0 0 3
After:  [0, 2, 2, 0]

Before: [1, 0, 2, 1]
6 3 2 0
After:  [1, 0, 2, 1]

Before: [0, 1, 2, 1]
6 3 2 2
After:  [0, 1, 1, 1]

Before: [2, 2, 1, 3]
10 1 3 3
After:  [2, 2, 1, 0]

Before: [0, 1, 0, 2]
15 1 3 1
After:  [0, 0, 0, 2]

Before: [2, 2, 2, 3]
10 2 3 1
After:  [2, 0, 2, 3]

Before: [3, 1, 1, 1]
14 1 3 1
After:  [3, 1, 1, 1]

Before: [2, 1, 2, 3]
1 1 2 1
After:  [2, 0, 2, 3]

Before: [3, 1, 2, 1]
1 1 2 3
After:  [3, 1, 2, 0]

Before: [0, 0, 3, 0]
0 0 3 1
After:  [0, 0, 3, 0]

Before: [1, 1, 1, 0]
12 1 3 0
After:  [1, 1, 1, 0]

Before: [2, 2, 2, 2]
11 2 3 3
After:  [2, 2, 2, 2]

Before: [1, 1, 2, 2]
11 2 3 0
After:  [2, 1, 2, 2]

Before: [2, 1, 0, 0]
12 1 3 0
After:  [1, 1, 0, 0]

Before: [2, 1, 2, 2]
8 3 2 0
After:  [0, 1, 2, 2]

Before: [2, 2, 0, 0]
13 0 3 2
After:  [2, 2, 1, 0]

Before: [3, 2, 3, 3]
4 3 2 1
After:  [3, 1, 3, 3]

Before: [0, 1, 1, 0]
2 2 1 1
After:  [0, 2, 1, 0]

Before: [3, 2, 0, 3]
4 3 0 1
After:  [3, 1, 0, 3]

Before: [3, 3, 3, 3]
4 3 0 2
After:  [3, 3, 1, 3]

Before: [3, 1, 0, 2]
8 0 2 1
After:  [3, 1, 0, 2]

Before: [2, 1, 0, 0]
12 1 3 3
After:  [2, 1, 0, 1]

Before: [0, 3, 2, 0]
7 0 0 2
After:  [0, 3, 0, 0]

Before: [3, 0, 2, 2]
11 2 3 2
After:  [3, 0, 2, 2]

Before: [1, 2, 2, 1]
6 3 2 1
After:  [1, 1, 2, 1]

Before: [1, 1, 1, 0]
12 1 3 3
After:  [1, 1, 1, 1]

Before: [3, 2, 2, 2]
11 2 3 3
After:  [3, 2, 2, 2]

Before: [0, 1, 2, 2]
7 0 0 0
After:  [0, 1, 2, 2]

Before: [0, 1, 2, 2]
7 0 0 1
After:  [0, 0, 2, 2]

Before: [0, 1, 2, 0]
12 1 3 3
After:  [0, 1, 2, 1]

Before: [2, 0, 3, 0]
5 3 2 3
After:  [2, 0, 3, 1]

Before: [1, 1, 2, 2]
1 1 2 0
After:  [0, 1, 2, 2]

Before: [1, 1, 2, 2]
3 0 2 1
After:  [1, 0, 2, 2]

Before: [3, 0, 3, 3]
4 3 0 1
After:  [3, 1, 3, 3]

Before: [2, 1, 3, 3]
10 1 3 3
After:  [2, 1, 3, 0]

Before: [1, 3, 2, 2]
3 0 2 0
After:  [0, 3, 2, 2]

Before: [3, 2, 2, 1]
6 3 2 0
After:  [1, 2, 2, 1]

Before: [3, 0, 1, 3]
10 2 3 3
After:  [3, 0, 1, 0]

Before: [3, 1, 2, 1]
9 3 3 2
After:  [3, 1, 0, 1]

Before: [2, 2, 3, 3]
4 3 2 0
After:  [1, 2, 3, 3]

Before: [3, 3, 0, 0]
8 0 2 2
After:  [3, 3, 1, 0]

Before: [2, 3, 3, 0]
13 0 3 0
After:  [1, 3, 3, 0]

Before: [1, 2, 2, 3]
3 0 2 1
After:  [1, 0, 2, 3]

Before: [3, 0, 1, 3]
4 3 0 0
After:  [1, 0, 1, 3]

Before: [2, 2, 3, 0]
13 0 3 3
After:  [2, 2, 3, 1]

Before: [2, 3, 0, 2]
5 2 3 2
After:  [2, 3, 1, 2]

Before: [2, 0, 3, 0]
13 0 3 1
After:  [2, 1, 3, 0]"""


inputTextProgram =
    """3 3 0 1
0 2 0 3
2 3 3 3
3 2 3 2
8 2 1 3
0 3 1 3
12 3 0 0
6 0 2 3
3 3 1 2
3 1 1 0
1 1 2 2
0 2 1 2
12 3 2 3
3 3 2 2
0 1 0 0
2 0 2 0
11 0 2 1
0 1 2 1
12 3 1 3
6 3 0 1
3 3 2 3
3 1 0 2
1 3 2 2
0 2 2 2
12 2 1 1
6 1 0 0
3 3 1 2
3 2 0 1
3 0 3 3
7 1 3 3
0 3 2 3
12 3 0 0
6 0 2 1
3 2 3 2
3 2 1 3
3 2 2 0
4 0 3 2
0 2 2 2
12 1 2 1
6 1 3 3
3 3 0 1
3 3 3 2
8 0 1 0
0 0 3 0
12 0 3 3
6 3 1 2
3 2 1 3
0 3 0 0
2 0 2 0
3 0 0 1
4 0 3 1
0 1 2 1
0 1 3 1
12 1 2 2
6 2 0 3
3 3 2 2
0 3 0 0
2 0 1 0
3 2 2 1
12 0 0 1
0 1 3 1
12 1 3 3
3 0 1 1
3 2 2 2
12 0 0 1
0 1 2 1
0 1 3 1
12 3 1 3
3 3 2 0
0 1 0 1
2 1 1 1
11 2 0 1
0 1 1 1
0 1 3 1
12 1 3 3
3 3 0 1
3 3 1 2
1 0 2 1
0 1 3 1
0 1 1 1
12 1 3 3
6 3 1 1
3 2 2 2
3 1 0 0
3 2 1 3
6 0 2 0
0 0 1 0
12 0 1 1
6 1 0 0
3 3 1 2
3 1 0 1
3 3 1 3
0 1 2 1
0 1 3 1
12 1 0 0
6 0 1 1
3 0 2 3
3 2 1 2
3 3 0 0
10 3 2 0
0 0 3 0
12 1 0 1
3 2 3 0
10 3 2 0
0 0 3 0
12 1 0 1
3 2 1 0
0 1 0 3
2 3 2 3
3 0 1 2
5 2 3 2
0 2 2 2
12 1 2 1
6 1 1 3
0 0 0 2
2 2 1 2
3 2 1 1
0 3 0 0
2 0 3 0
11 1 0 1
0 1 1 1
12 3 1 3
6 3 0 1
3 1 3 3
3 2 1 0
14 3 0 3
0 3 1 3
12 1 3 1
6 1 2 2
3 2 2 1
3 3 2 3
0 0 0 0
2 0 3 0
11 1 0 3
0 3 1 3
0 3 3 3
12 3 2 2
6 2 1 3
3 3 1 1
3 1 3 2
1 0 2 1
0 1 1 1
12 1 3 3
6 3 2 1
3 3 2 2
0 0 0 3
2 3 1 3
3 1 0 0
12 0 0 3
0 3 1 3
12 1 3 1
6 1 2 2
3 0 3 0
3 3 3 1
3 1 1 3
2 3 1 3
0 3 2 3
12 2 3 2
6 2 0 3
3 3 2 2
3 2 0 1
3 2 0 0
9 0 2 2
0 2 2 2
0 2 3 2
12 3 2 3
3 1 2 2
3 3 2 1
8 0 1 0
0 0 1 0
0 0 2 0
12 3 0 3
3 0 0 1
0 0 0 0
2 0 1 0
3 0 2 2
12 0 0 0
0 0 2 0
12 3 0 3
3 3 2 1
3 3 2 0
9 2 0 2
0 2 2 2
12 3 2 3
6 3 0 2
0 0 0 0
2 0 0 0
3 2 2 3
3 1 2 1
14 1 3 3
0 3 1 3
0 3 3 3
12 3 2 2
6 2 1 1
3 2 1 0
3 2 3 3
3 1 3 2
4 0 3 0
0 0 2 0
12 0 1 1
6 1 2 0
0 0 0 3
2 3 3 3
3 2 2 1
0 2 0 2
2 2 2 2
15 3 1 2
0 2 3 2
12 0 2 0
3 0 3 2
0 0 0 3
2 3 0 3
7 1 3 1
0 1 1 1
12 0 1 0
3 3 3 1
3 3 2 2
5 3 2 3
0 3 1 3
12 0 3 0
6 0 1 3
0 1 0 0
2 0 2 0
3 2 0 1
11 1 2 1
0 1 3 1
12 1 3 3
3 3 1 1
0 3 0 2
2 2 2 2
3 1 1 0
6 0 2 0
0 0 1 0
12 0 3 3
6 3 1 0
3 0 2 3
10 3 2 3
0 3 2 3
12 3 0 0
3 2 1 3
3 0 0 1
3 0 1 2
5 2 3 3
0 3 3 3
12 0 3 0
6 0 0 3
3 2 3 2
3 1 2 0
2 0 1 2
0 2 1 2
12 3 2 3
3 3 2 1
3 0 0 2
2 0 1 1
0 1 1 1
12 1 3 3
6 3 1 2
3 2 3 0
3 0 1 1
3 1 2 3
13 0 3 0
0 0 2 0
0 0 2 0
12 0 2 2
6 2 3 1
0 2 0 2
2 2 2 2
3 0 3 3
3 3 0 0
10 3 2 2
0 2 2 2
12 1 2 1
0 2 0 0
2 0 2 0
3 3 2 2
0 1 0 3
2 3 1 3
9 0 2 3
0 3 3 3
12 1 3 1
3 0 2 3
3 0 0 2
3 3 0 0
0 0 2 0
0 0 1 0
12 1 0 1
6 1 2 0
3 1 0 2
3 3 1 3
0 2 0 1
2 1 2 1
1 3 2 3
0 3 2 3
12 0 3 0
6 0 1 3
3 1 0 0
3 2 3 2
3 1 0 1
6 0 2 1
0 1 2 1
12 1 3 3
6 3 2 2
3 2 0 1
0 1 0 0
2 0 2 0
3 3 2 3
15 3 0 3
0 3 1 3
12 3 2 2
6 2 3 1
3 1 3 3
3 0 3 2
0 2 0 0
2 0 1 0
12 3 3 2
0 2 1 2
0 2 3 2
12 1 2 1
6 1 1 2
0 1 0 1
2 1 0 1
0 3 0 0
2 0 2 0
0 3 0 3
2 3 2 3
4 0 3 3
0 3 1 3
12 3 2 2
6 2 3 3
3 3 2 1
3 3 0 2
11 0 2 0
0 0 2 0
0 0 3 0
12 0 3 3
3 2 1 0
9 0 2 1
0 1 1 1
0 1 1 1
12 3 1 3
6 3 0 0
3 0 0 2
3 2 3 3
3 1 1 1
0 1 2 2
0 2 2 2
0 2 3 2
12 2 0 0
6 0 2 1
3 3 0 0
3 2 1 2
7 2 3 3
0 3 3 3
12 3 1 1
6 1 1 2
3 1 1 0
3 2 0 3
0 3 0 1
2 1 0 1
2 0 1 1
0 1 3 1
12 2 1 2
6 2 2 1
3 2 1 2
3 0 2 3
3 3 0 0
0 0 2 0
0 0 3 0
12 1 0 1
6 1 1 0
3 2 0 3
0 1 0 1
2 1 2 1
3 0 1 2
5 2 3 1
0 1 2 1
12 0 1 0
6 0 3 2
3 2 1 1
3 2 0 0
4 0 3 3
0 3 1 3
12 3 2 2
6 2 0 1
3 3 0 2
3 2 3 3
4 0 3 0
0 0 3 0
0 0 1 0
12 0 1 1
0 2 0 0
2 0 2 0
3 1 1 3
14 3 0 2
0 2 3 2
12 1 2 1
6 1 3 2
3 1 1 1
13 0 3 3
0 3 3 3
0 3 3 3
12 2 3 2
3 2 3 1
3 2 1 3
4 0 3 1
0 1 2 1
12 2 1 2
6 2 3 3
3 2 1 1
3 3 2 2
11 0 2 0
0 0 1 0
12 0 3 3
6 3 2 2
3 1 1 0
3 2 1 3
14 0 3 3
0 3 1 3
12 3 2 2
6 2 1 1
3 2 2 2
0 3 0 3
2 3 2 3
3 2 3 0
4 0 3 3
0 3 2 3
12 1 3 1
6 1 3 0
3 1 3 2
3 2 2 3
3 2 2 1
7 1 3 3
0 3 3 3
0 3 2 3
12 0 3 0
6 0 1 2
3 1 3 3
3 3 3 1
3 2 3 0
13 0 3 0
0 0 2 0
12 2 0 2
6 2 1 0
3 0 1 3
3 2 2 2
3 1 1 1
7 2 3 1
0 1 2 1
12 1 0 0
6 0 1 1
3 1 2 2
0 0 0 0
2 0 2 0
7 0 3 2
0 2 2 2
12 1 2 1
6 1 0 3
3 1 0 0
3 0 2 1
3 2 1 2
6 0 2 2
0 2 2 2
12 2 3 3
6 3 0 2
3 1 2 3
2 3 1 1
0 1 3 1
12 1 2 2
6 2 0 0
0 3 0 2
2 2 0 2
3 3 0 3
3 3 2 1
3 2 1 2
0 2 1 2
12 2 0 0
6 0 3 1
3 3 3 2
3 1 0 0
3 2 0 3
14 0 3 2
0 2 2 2
12 1 2 1
6 1 1 2
3 3 1 3
3 1 2 1
3 2 2 0
15 3 0 3
0 3 2 3
12 3 2 2
3 3 2 3
3 2 2 1
15 3 1 1
0 1 1 1
12 1 2 2
3 1 0 0
3 2 0 3
3 2 1 1
14 0 3 0
0 0 1 0
0 0 3 0
12 0 2 2
6 2 0 1
3 1 1 3
3 2 2 2
3 2 2 0
13 0 3 3
0 3 2 3
12 3 1 1
3 1 3 0
3 0 1 3
6 0 2 0
0 0 3 0
12 1 0 1
6 1 0 2
3 2 0 0
3 2 0 3
3 3 2 1
4 0 3 1
0 1 2 1
12 1 2 2
6 2 2 0
0 3 0 1
2 1 3 1
0 0 0 3
2 3 1 3
3 1 3 2
2 3 1 1
0 1 1 1
0 1 3 1
12 0 1 0
6 0 0 2
3 1 1 1
3 2 3 0
3 0 1 3
14 1 0 0
0 0 1 0
12 2 0 2
6 2 0 0
3 2 2 3
3 2 3 1
3 3 3 2
11 1 2 3
0 3 2 3
12 0 3 0
6 0 0 1
3 0 1 3
0 3 0 0
2 0 2 0
3 2 0 2
10 3 2 2
0 2 3 2
0 2 3 2
12 1 2 1
3 0 1 2
3 1 0 3
3 3 3 0
1 0 2 0
0 0 3 0
0 0 3 0
12 1 0 1
6 1 1 3
3 3 2 2
3 1 1 0
0 0 0 1
2 1 0 1
2 0 1 2
0 2 3 2
12 3 2 3
6 3 1 1
0 1 0 3
2 3 2 3
3 2 3 0
3 2 2 2
7 2 3 2
0 2 3 2
12 2 1 1
6 1 1 3
0 1 0 1
2 1 3 1
3 1 2 0
3 2 0 2
6 0 2 2
0 2 3 2
12 3 2 3
6 3 0 1
0 2 0 2
2 2 2 2
3 1 0 3
3 2 3 0
14 3 0 0
0 0 3 0
12 1 0 1
6 1 3 0
3 3 2 2
0 0 0 1
2 1 1 1
0 3 0 3
2 3 2 3
0 1 2 2
0 2 1 2
12 0 2 0
6 0 2 1
3 1 2 3
3 1 1 2
3 2 1 0
12 3 3 3
0 3 2 3
12 3 1 1
6 1 0 0
3 0 0 3
3 0 3 1
3 3 2 2
3 3 1 2
0 2 3 2
0 2 3 2
12 2 0 0
3 1 3 3
3 1 0 2
3 3 1 1
12 3 3 3
0 3 1 3
12 3 0 0
6 0 1 3
3 2 3 1
3 0 2 2
3 3 0 0
9 2 0 0
0 0 2 0
12 3 0 3
6 3 0 2
3 2 1 0
3 3 0 1
3 2 2 3
4 0 3 0
0 0 2 0
12 0 2 2
0 1 0 0
2 0 2 0
3 0 0 1
4 0 3 0
0 0 1 0
12 2 0 2
6 2 2 1
3 2 1 0
3 3 1 2
11 0 2 3
0 3 2 3
12 3 1 1
3 1 1 0
3 0 2 3
3 0 0 2
0 0 2 2
0 2 2 2
0 2 1 2
12 2 1 1
6 1 1 2
3 0 1 0
3 2 2 1
7 1 3 0
0 0 3 0
12 2 0 2
6 2 0 3
3 1 2 2
3 3 2 0
11 1 0 2
0 2 3 2
0 2 1 2
12 2 3 3
6 3 1 2
3 0 2 0
0 2 0 3
2 3 2 3
7 1 3 1
0 1 2 1
12 2 1 2
6 2 2 3
0 0 0 1
2 1 3 1
3 3 3 2
3 2 1 0
15 1 0 2
0 2 3 2
12 3 2 3
3 2 0 1
3 2 0 2
3 3 3 0
11 1 0 2
0 2 2 2
0 2 2 2
12 3 2 3
6 3 1 1
0 0 0 0
2 0 0 0
3 2 1 3
3 0 0 2
5 2 3 2
0 2 1 2
12 1 2 1
6 1 0 3
3 3 2 1
3 2 2 0
3 3 2 2
9 0 2 0
0 0 1 0
12 0 3 3
3 3 1 0
3 0 1 1
3 0 0 2
9 2 0 2
0 2 3 2
12 2 3 3
6 3 2 2
3 3 0 3
3 2 2 1
3 2 3 0
15 3 0 1
0 1 3 1
12 1 2 2
0 1 0 1
2 1 3 1
3 2 3 3
8 0 1 0
0 0 1 0
0 0 3 0
12 0 2 2
6 2 3 1
3 2 2 0
3 3 1 2
3 1 3 3
13 0 3 0
0 0 2 0
12 1 0 1
6 1 3 0
3 0 3 2
3 2 0 1
0 3 2 1
0 1 3 1
12 0 1 0
6 0 2 2
3 2 2 1
3 2 1 0
13 0 3 1
0 1 2 1
12 1 2 2
6 2 1 0
3 0 0 3
3 2 1 2
3 2 1 1
10 3 2 2
0 2 1 2
12 2 0 0
6 0 2 3
3 1 0 0
0 0 0 2
2 2 2 2
6 0 2 1
0 1 2 1
12 3 1 3
6 3 3 0
0 2 0 3
2 3 1 3
3 3 0 1
8 2 1 3
0 3 1 3
12 0 3 0
6 0 1 1
3 2 3 3
0 3 0 0
2 0 3 0
0 0 0 2
2 2 0 2
5 2 3 0
0 0 1 0
12 1 0 1
6 1 3 3
3 2 2 1
3 2 1 2
3 3 0 0
8 2 0 0
0 0 2 0
0 0 1 0
12 3 0 3
3 3 3 2
0 0 0 0
2 0 1 0
3 0 2 1
12 0 0 0
0 0 2 0
12 0 3 3
6 3 0 2
3 1 3 0
3 1 3 3
3 1 0 1
3 3 0 0
0 0 3 0
12 2 0 2
6 2 1 1
3 1 2 0
3 2 1 2
3 0 1 3
10 3 2 2
0 2 1 2
12 2 1 1
6 1 1 0
0 2 0 2
2 2 2 2
3 0 2 1
7 2 3 3
0 3 3 3
12 3 0 0
3 0 3 2
3 1 2 1
0 3 0 3
2 3 3 3
0 1 2 3
0 3 1 3
0 3 1 3
12 3 0 0
6 0 1 1
3 0 2 0
0 3 0 3
2 3 3 3
3 2 0 0
0 0 2 0
12 1 0 1
6 1 0 3
3 3 1 2
3 2 2 0
3 0 0 1
9 0 2 1
0 1 3 1
12 3 1 3
6 3 3 1
3 0 2 3
3 1 3 0
3 2 2 2
10 3 2 0
0 0 3 0
12 0 1 1
6 1 1 0
0 2 0 1
2 1 0 1
3 2 3 3
3 0 1 2
5 2 3 1
0 1 3 1
0 1 1 1
12 1 0 0
3 3 2 1
5 2 3 1
0 1 2 1
0 1 2 1
12 0 1 0
6 0 0 1
3 1 2 0
14 0 3 0
0 0 2 0
12 0 1 1
3 3 1 3
3 3 1 2
3 0 3 0
1 3 2 2
0 2 1 2
12 1 2 1
6 1 2 3
3 1 2 0
3 3 3 1
3 2 0 2
6 0 2 1
0 1 3 1
12 1 3 3
6 3 3 0
0 0 0 1
2 1 0 1
0 1 0 3
2 3 2 3
0 2 0 2
2 2 0 2
5 2 3 2
0 2 3 2
0 2 3 2
12 0 2 0
0 2 0 1
2 1 1 1
0 1 0 2
2 2 0 2
5 2 3 3
0 3 3 3
12 0 3 0
6 0 3 1
3 2 2 3
3 2 3 2
3 3 0 0
7 2 3 2
0 2 3 2
12 1 2 1
6 1 1 3
3 2 1 0
3 3 1 2
3 1 1 1
14 1 0 0
0 0 2 0
12 0 3 3
6 3 0 0
0 1 0 3
2 3 0 3
3 2 0 2
0 3 0 1
2 1 0 1
10 3 2 3
0 3 2 3
12 0 3 0
6 0 3 2
3 2 3 1
3 2 2 0
3 1 2 3
13 0 3 0
0 0 1 0
0 0 3 0
12 2 0 2
6 2 2 0
3 3 0 2
3 0 1 1
3 2 3 2
0 2 2 2
12 2 0 0
6 0 0 2
3 1 0 0
0 2 0 1
2 1 3 1
3 2 0 3
2 0 1 0
0 0 2 0
12 0 2 2
6 2 2 1
3 0 0 3
3 2 1 0
0 0 0 2
2 2 3 2
5 3 2 2
0 2 3 2
12 1 2 1
6 1 0 2
3 1 0 1
3 3 0 3
14 1 0 1
0 1 1 1
12 1 2 2
6 2 2 1
3 0 0 2
0 1 0 3
2 3 1 3
13 0 3 0
0 0 2 0
12 0 1 1
6 1 0 2
3 2 1 0
3 3 2 1
13 0 3 1
0 1 1 1
12 2 1 2
6 2 0 3
3 2 1 2
3 2 1 1
0 3 0 0
2 0 3 0
11 2 0 1
0 1 1 1
12 3 1 3
6 3 0 1
3 3 2 3
0 0 0 0
2 0 1 0
6 0 2 2
0 2 3 2
12 1 2 1
6 1 0 3
3 3 3 1
3 1 0 2
1 1 2 2
0 2 1 2
12 2 3 3
6 3 0 1
3 0 2 0
0 2 0 2
2 2 2 2
0 1 0 3
2 3 0 3
10 3 2 0
0 0 3 0
12 0 1 1
3 1 0 0
6 0 2 2
0 2 1 2
12 2 1 1
6 1 1 2
3 2 3 0
0 0 0 3
2 3 2 3
0 2 0 1
2 1 2 1
4 0 3 0
0 0 3 0
12 2 0 2
6 2 2 1
3 1 3 3
3 1 0 2
0 2 0 0
2 0 2 0
13 0 3 3
0 3 1 3
12 1 3 1
6 1 2 2
0 3 0 0
2 0 1 0
3 3 0 3
3 1 0 1
12 1 0 1
0 1 3 1
0 1 3 1
12 2 1 2
3 2 1 0
3 1 3 1
3 1 0 3
13 0 3 1
0 1 3 1
0 1 2 1
12 1 2 2
6 2 2 1
3 1 2 0
3 2 1 2
6 0 2 0
0 0 3 0
12 0 1 1
6 1 1 0
3 3 2 2
3 1 3 1
0 3 2 3
0 3 1 3
12 0 3 0
3 0 3 3
3 2 0 2
10 3 2 2
0 2 2 2
12 0 2 0
3 3 0 2
3 1 2 3
3 2 1 2
0 2 1 2
12 2 0 0"""
