module MomoRisc.Cpu exposing
  ( Cpu
  , MemoryAccessPhaseAction(..)
  , init
  , step
  )

import MomoRisc.Dudit as Dudit exposing (Dudit, zero, one, add, sub, mul, muh, toInt)
import MomoRisc.Inst as Inst exposing (Inst(..))
import MomoRisc.Program as Program exposing (Program)
import MomoRisc.Reg as Reg exposing (Reg(..))



type alias Cpu =
  { pc : Dudit
  , a : Dudit
  , b : Dudit
  , c : Dudit
  , d : Dudit
  }


type MemoryAccessPhaseAction
  = NoAccessNeeded Cpu
  | ReadNeeded Dudit (Dudit -> Cpu)
  | WriteNeeded Dudit Dudit Cpu


init : Cpu
init =
  { pc = zero
  , a = zero
  , b = zero
  , c = zero
  , d = zero
  }


step : Program -> Cpu -> MemoryAccessPhaseAction
step prog cpu =
  case Program.fetch cpu.pc prog of
    LDI rd imm ->
      let
        cpu_ =
          cpu
            |> write rd imm
            |> incPc
      in
        NoAccessNeeded cpu_

    LD rd rs ->
      let
        addr = read rs cpu
        cpu_ data =
          cpu
            |> write rd data
            |> incPc
      in
        ReadNeeded addr cpu_

    ST rs2 rs1 ->
      let
        addr = read rs1 cpu
        data = read rs2 cpu
        cpu_ = incPc cpu
      in
        WriteNeeded addr data cpu_

    ADD rd rs1 rs2 ->
      let
        o1 = read rs1 cpu
        o2 = read rs2 cpu
        r = add o1 o2
        cpu_ =
          cpu
            |> write rd r
            |> incPc
      in
        NoAccessNeeded cpu_

    ADI rd rs imm ->
      let
        o1 = read rs cpu
        o2 = imm
        r = add o1 o2
        cpu_ =
          cpu
            |> write rd r
            |> incPc
      in
        NoAccessNeeded cpu_

    SUB rd rs1 rs2 ->
      let
        o1 = read rs1 cpu
        o2 = read rs2 cpu
        r = sub o1 o2
        cpu_ =
          cpu
            |> write rd r
            |> incPc
      in
        NoAccessNeeded cpu_

    MUL rd rs1 rs2 ->
      let
        o1 = read rs1 cpu
        o2 = read rs2 cpu
        r = mul o1 o2
        cpu_ =
          cpu
            |> write rd r
            |> incPc
      in
        NoAccessNeeded cpu_

    MUH rd rs1 rs2 ->
      let
        o1 = read rs1 cpu
        o2 = read rs2 cpu
        r = muh o1 o2
        cpu_ =
          cpu
            |> write rd r
            |> incPc
      in
        NoAccessNeeded cpu_

    BEQ rs1 rs2 imm ->
      let
        v1 = read rs1 cpu |> toInt
        v2 = read rs2 cpu |> toInt
        pc =
          if v1 == v2
          then imm
          else add cpu.pc one
        cpu_ = { cpu | pc = pc }
      in
        NoAccessNeeded cpu_

    BLT rs1 rs2 imm ->
      let
        v1 = read rs1 cpu |> toInt
        v2 = read rs2 cpu |> toInt
        pc =
          if v1 < v2
          then imm
          else add cpu.pc one
        cpu_ = { cpu | pc = pc }
      in
        NoAccessNeeded cpu_

    JPI rd imm ->
      let
        cpu_ = write rd (add cpu.pc one) cpu
        cpu__ = { cpu_ | pc = imm }
      in
        NoAccessNeeded cpu__

    JP rd rs ->
      let
        addr = read rs cpu
        cpu_ = write rd (add cpu.pc one) cpu
        cpu__ = { cpu_ | pc = addr }
      in
        NoAccessNeeded cpu__

    HLT ->
      NoAccessNeeded cpu


read : Reg -> Cpu -> Dudit
read reg cpu =
  case reg of
    A -> cpu.a
    B -> cpu.b
    C -> cpu.c
    D -> cpu.d


write : Reg -> Dudit -> Cpu -> Cpu
write reg value cpu =
  case reg of
    A -> { cpu | a = value }
    B -> { cpu | b = value }
    C -> { cpu | c = value }
    D -> { cpu | d = value }


incPc : Cpu -> Cpu
incPc cpu =
  { cpu | pc = add cpu.pc one }
