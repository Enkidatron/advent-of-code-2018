module Day19 exposing (Machine, OpCode, Operation(..), Registers, boolToInt, initMachine, inputText, ipRegister, opCodeOperationParser, operationParser, parseProgram, part1, part2, performOperation, programDescr2Part2, programDescription, programParser, psuedoCode, readRegister, runMachine, shortcut, stepMachine)

import Array exposing (Array)
import Bitwise
import Parser exposing ((|.), (|=), Parser)


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


type alias OpCode a =
    { op : a
    , a : Int
    , b : Int
    , c : Int
    }


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
    Parser.oneOf
        [ Parser.succeed AddR
            |. Parser.symbol "addr"
        , Parser.succeed AddI
            |. Parser.symbol "addi"
        , Parser.succeed MulR
            |. Parser.symbol "mulr"
        , Parser.succeed MulI
            |. Parser.symbol "muli"
        , Parser.succeed BAnR
            |. Parser.symbol "banr"
        , Parser.succeed BAnI
            |. Parser.symbol "bani"
        , Parser.succeed BOrR
            |. Parser.symbol "borr"
        , Parser.succeed BOrI
            |. Parser.symbol "bori"
        , Parser.succeed SetR
            |. Parser.symbol "setr"
        , Parser.succeed SetI
            |. Parser.symbol "seti"
        , Parser.succeed GTIR
            |. Parser.symbol "gtir"
        , Parser.succeed GTRI
            |. Parser.symbol "gtri"
        , Parser.succeed GTRR
            |. Parser.symbol "gtrr"
        , Parser.succeed EQIR
            |. Parser.symbol "eqir"
        , Parser.succeed EQRI
            |. Parser.symbol "eqri"
        , Parser.succeed EQRR
            |. Parser.symbol "eqrr"
        ]


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


parseProgram text =
    Parser.run programParser text |> Result.withDefault [] |> Array.fromList


type alias Machine =
    { program : Array (OpCode Operation)
    , ip : Int
    , registers : Registers
    }


initMachine text =
    { program = parseProgram text
    , ip = 0
    , registers = Array.repeat 6 0
    }


runMachine machine =
    let
        nextCommand =
            Array.get machine.ip machine.program
    in
    case nextCommand of
        Nothing ->
            machine

        Just command ->
            runMachine (stepMachine command machine)


stepMachine command machine =
    let
        newRegisters =
            machine.registers
                |> Array.set ipRegister machine.ip
                |> performOperation command
                |> Debug.log "registers"

        newIp =
            Array.get ipRegister newRegisters
                |> Maybe.withDefault -1000
                |> (\x -> x + 1)
    in
    { machine | registers = newRegisters, ip = newIp }


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


ipRegister =
    1


part1 text =
    initMachine text |> runMachine |> .registers |> Array.get 0


part2 text =
    initMachine text
        |> (\m -> { m | registers = Array.fromList [ 1, 0, 0, 0, 0, 0 ] })
        |> runMachine
        |> .registers
        |> Array.get 0


inputText =
    """addi 1 16 1
seti 1 5 5
seti 1 2 3
mulr 5 3 2
eqrr 2 4 2
addr 2 1 1
addi 1 1 1
addr 5 0 0
addi 3 1 3
gtrr 3 4 2
addr 1 2 1
seti 2 6 1
addi 5 1 5
gtrr 5 4 2
addr 2 1 1
seti 1 8 1
mulr 1 1 1
addi 4 2 4
mulr 4 4 4
mulr 1 4 4
muli 4 11 4
addi 2 5 2
mulr 2 1 2
addi 2 12 2
addr 4 2 4
addr 1 0 1
seti 0 4 1
setr 1 4 2
mulr 2 1 2
addr 1 2 2
mulr 1 2 2
muli 2 14 2
mulr 2 1 2
addr 4 2 4
seti 0 3 0
seti 0 7 1"""


psuedoCode =
    """
    0: goto 17 
    1: Set r5 = 1
    2: Set r3 = 1
    3: Set r2 = r3 * r5
    4: Set r2 = (r4 == r2)
    5: if r2, goto 7
    6: goto 8
    7: Set r0 = r0 + r5
    8: r3 += 1
    9: Set r2 = (r3 > r4)
    10: if r2, goto 12
    11: goto 3
    12: r5 += 1
    13: Set r2 = (r5 > r4)
    14: if r2, goto 16
    15: goto 2
    16: exit
    17: r4 += 2
    18: r4 = r4 ^ 2
    19: r4 *= 19
    20: r4 *= 11
    21: r2 += 5
    22: r2 *= 22
    23: r2 += 12
    24: r4 = r4 + r2
    25: r1 = r1 + r0 (jumpby r0)
    26: goto 1
    27: Set r2 = 27
    28: r2 *= 28
    29: r2 += 29
    30: r2 *= 30
    31: r2 *= 14
    32: r2 *= 32
    33: r4 += r2
    34: Set r0 = 0
    35: goto 1
    """


programDescription =
    """
    start: goto init 
    loopReset: r3 = 1
    r5 = 1
    loop: 
    if (r3 * r5 == r4) then 
        r0 += r5
    
    
    r3 += 1
    if (r3 <= r4) then 
        goto loop
    else 
        r5 += 1
        if (r5 > r4) then 
            exit
        else 
            r3 = 1
            goto loop 
    init: 
    r4 = (((r4 + 2)^2) * 19 * 11) + 122
    if (part1) then 
        goto loopReset
    else (part2) 
        r4 += 10550400
        goto loopReset
    """


programDescr2Part2 =
    """
    (part1 r4 = 958)
    r4 = 10551358
    Find all prime factors of r4
    and sum them together
    """


shortcut r4 =
    List.range 1 r4
        |> List.filter (\x -> Basics.remainderBy x r4 == 0)
        |> List.sum
