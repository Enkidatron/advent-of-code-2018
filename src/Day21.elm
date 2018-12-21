module Day21 exposing (inputText, ipRegister, part1, part2)

import Array exposing (Array)
import Day19 exposing (..)
import Set exposing (..)


part1 text =
    initMachine text
        -- I saw that command 28 was the only place where register 0 could make a difference,
        -- and that it was comparing r0 to r4.
        -- So I made a version of runMachine that logs the value of r4 as soon as we get to command 28
        -- this code is just to confirm that it exits with this answer in r0
        |> (\m -> { m | registers = Array.fromList [ 10961197, 0, 0, 0, 0, 0 ] })
        |> runMachine


part2 text =
    initMachine text
        |> findR4BeforeLoop 0 Set.empty


findR4BeforeLoop lastR4 seenR4s machine =
    let
        -- This is not fast. It took about 20-30 minutes to find the answer
        -- but this was faster than I could come up with a better solution.
        nextCommand =
            Array.get machine.ip machine.program

        r4 =
            if machine.ip == 28 then
                Array.get 4 machine.registers
                    |> Maybe.withDefault -1
                    |> Debug.log "r4"

            else
                lastR4

        newR4s =
            if machine.ip == 28 then
                Set.insert r4 seenR4s

            else
                seenR4s
    in
    if machine.ip == 29 && Set.member r4 seenR4s then
        { firstLoop = Just r4, prevR4 = Just lastR4, message = "found loop" }

    else
        case nextCommand of
            Nothing ->
                { firstLoop = Nothing, prevR4 = Just lastR4, message = "exited without loop" }

            Just command ->
                findR4BeforeLoop r4 newR4s (stepMachine command machine)


runMachine machine =
    let
        nextCommand =
            Array.get machine.ip machine.program

        _ =
            if machine.ip == 28 then
                Array.get 4 machine.registers
                    |> Debug.log "r4"

            else
                Nothing
    in
    case nextCommand of
        Nothing ->
            Debug.log "exiting" machine

        Just command ->
            runMachine (stepMachine command machine)


stepMachine command machine =
    let
        newRegisters =
            machine.registers
                |> Array.set ipRegister machine.ip
                |> performOperation command

        newIp =
            Array.get ipRegister newRegisters
                |> Maybe.withDefault -1000
                |> (\x -> x + 1)
    in
    { machine | registers = newRegisters, ip = newIp }


ipRegister =
    3


inputText =
    """seti 123 0 4
bani 4 456 4
eqri 4 72 4
addr 4 3 3
seti 0 0 3
seti 0 6 4
bori 4 65536 1
seti 678134 1 4
bani 1 255 5
addr 4 5 4
bani 4 16777215 4
muli 4 65899 4
bani 4 16777215 4
gtir 256 1 5
addr 5 3 3
addi 3 1 3
seti 27 8 3
seti 0 1 5
addi 5 1 2
muli 2 256 2
gtrr 2 1 2
addr 2 3 3
addi 3 1 3
seti 25 7 3
addi 5 1 5
seti 17 1 3
setr 5 3 1
seti 7 8 3
eqrr 4 0 5
addr 5 3 3
seti 5 4 3"""


pseudocode =
    """0: Set r4 = 123 0b001111011
    1: r4 = r4 && 456  0b111001000 -> 0b001001000 = 72
    2: r4 = r4 == 72  (=1)
    3: r3 += r4 (skip next if r4 is true)
    4: goto 1
    5: Set r4 = 0
    6: Set r1 = r4 || 65536 0b10000000000000000
    7: Set r4 =      678134 0b000010100101100011110110
    8: Set r5 = r1 && 255 (=0)
    9: r4 += r5 
    10: r4 = r4 && 16777215 0b111111111111111111111111
    11: r4 *= 65899 (=0b101001100111101000100010010011010010)
    12: r4 = r4 && 16777215       0b111111111111111111111111  (=0b101000100010010011010010)
    13: r5 = (256 > r1) (=false)
    14: if r5, goto 16
    15: goto 17
    16: goto 28
    17: Set r5 = 0
    18: Set r2 = r5 + 1
    19: r2 *= 256
    20: r2 = (r2 > r1)
    21: if r2== true, goto 23
    22: goto 24
    23: goto 26
    24: r5 += 1
    25: goto 18
    26: r1 = r5
    27: goto 8
    28: r5 = r0 == r4
    29: if r5 == true, exit
    30: goto 6
    """


programDescription =
    """
initial testings stuff



start: r4 = 0
outerloopstart: r1 = 0b10000000000000000 || r4
r4 = 0b000010100101100011110110
r5 = 0b11111111 && r1
r4 += r5
r4 = r4 && 0b111111111111111111111111
r4 *= 65889
r4 = r4 && 0b111111111111111111111111
etc...
    """
