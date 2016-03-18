{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- For deriving Num for Literal and Offset
-- |
-- Module      : HBPF.Internal
-- Description : Internal types for BPF assembly representation and pretty printing
-- Copyright   : (c) Andrew Duffy and Matt Denton, 2016
-- License     : BSD3
-- Maintainer  : agd@cs.stanford.edu
-- Stability   : experimental
-- Portability : unknown
--
-- Internal types
--
module HBPF.Internal where
--  (
--    ld , ldh , ldx , ldxb
--  , st , stx
--  , andI , orI , sub
--  , jmp , ja , jeq , jneq , jgt , jge
--  , tax , txa
--  , ret , retA
--  , LoadAddress(..)
--  , StoreAddress(..)
--  , ArithAddress(..)
--  , ReturnAddress(..)
--  , ConditionalJmpAddress(..)
--  , toString
--  , label
--  , Instr(..)
--  , NamedRegister(..)
--  , IndexedRegister(..)
--  ) where

import Data.String (IsString)

--
-- * Representations of registers, both named registers and the 16 indexed registers
--

-- |Named registers. BPF's virtual machine only has two: an accumulator A and an auxiliary X
data NamedRegister = A | X

-- |Indexed registers. BPF's virtual machine provides 16 general purpose registers
data IndexedRegister = M0 | M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9 | M10 | M11 | M12 | M13 | M14 | M15

--
-- * Domain-specific versions of primitives
--

-- | Represents a numeric Literal (immediate value)
newtype Literal = Literal { unLiteral :: Int } deriving (Eq, Show, Num)

-- | Representation of a byte offset
newtype Offset = Offset { unOffset :: Int } deriving (Eq, Show, Num)

-- | Representation of a label that appears in the assembly
newtype Label = Label { unLabel :: String } deriving (Eq, Show, IsString)

--
-- * Addressing Types
--

-- | Addressing options for the @ld*@ family of instructions
data LoadAddress = LoadAddrLiteral Literal
                 | LoadAddrByteOff Offset
                 | LoadAddrNamedRegister NamedRegister
                 | LoadAddrIndexedRegister IndexedRegister
                 | LoadAddrNibble Offset -- only used for ldxb and ldx
                 | LoadAddrOffFromX Offset -- only for ld, ldh, ldb

-- | Addressing options for the @st@ and @stx@ instructions
data StoreAddress = StoreAddrIndexedRegister IndexedRegister

data ReturnAddress = ReturnAddrLiteral Literal
                   | ReturnAddrA NamedRegister

-- |Sum type of addressing options available for arithmetic instructions
data ArithAddress = ArithAddrLiteral Literal
                  | ArithAddrRegister NamedRegister


-- | Sum type of addressing options available for unconditional jump instructions
data UnconditionalJmpAddress = UnconditionalJmpAddrLabel Label

-- | Addressing options available for conditional jumps
data ConditionalJmpAddress = ConditionalJmpAddrLiteralTrue Literal Label
                           | ConditionalJmpAddrLiteralTrueFalse Literal Label Label

-- | Different load sizes
data LoadType = LoadWord | LoadHalfWord | LoadByte

--
-- * Instructions
--

-- |BPF Instruction set as an ADT:
data BPFInstr =
              -- Load and store instructions
                ILoad NamedRegister LoadType LoadAddress
              | IStore NamedRegister StoreAddress
              -- Arithmetic instructions
              | ILeftShift ArithAddress
              | IRightShift ArithAddress
              | IAnd ArithAddress
              | IOr ArithAddress
              | IMod ArithAddress
              | IMult ArithAddress
              | IDiv ArithAddress
              | IAdd ArithAddress
              | ISub ArithAddress
              -- Conditional/Unconditional Jump instructions
              | IJmp UnconditionalJmpAddress
              | IJAbove UnconditionalJmpAddress
              | IJEq ConditionalJmpAddress
              | IJNotEq ConditionalJmpAddress
              | IJGreater ConditionalJmpAddress
              | IJGreaterEq ConditionalJmpAddress
              -- Misc
              | ISwapAX
              | ISwapXA
              | IRet ReturnAddress

-- For ease of use
ld :: LoadAddress -> [Instr]
ld addr = [IInstr $ ILoad A LoadWord addr]

ldh :: LoadAddress -> [Instr]
ldh addr = [IInstr $ ILoad A LoadHalfWord addr]

ldx :: LoadAddress -> [Instr]
ldx addr = [IInstr $ ILoad X LoadWord addr]

ldxb :: LoadAddress -> [Instr]
ldxb addr = [IInstr $ ILoad X LoadByte addr]

st :: StoreAddress -> [Instr]
st addr = [IInstr $ IStore A addr]

stx :: StoreAddress -> [Instr]
stx addr = [IInstr $ IStore X addr]

andI :: ArithAddress -> [Instr]
andI addr = [IInstr $ IAnd addr]

orI :: ArithAddress -> [Instr]
orI addr = [IInstr $ IOr addr]

sub :: ArithAddress -> [Instr]
sub addr = [IInstr $ ISub addr]

jmp :: UnconditionalJmpAddress -> [Instr]
jmp a = [IInstr $ IJmp a]

ja :: UnconditionalJmpAddress -> [Instr]
ja a = [IInstr $ IJAbove a]

jeq :: ConditionalJmpAddress -> [Instr]
jeq a = [IInstr $ IJEq a]

jneq :: ConditionalJmpAddress -> [Instr]
jneq a = [IInstr $ IJNotEq a]

jgt :: ConditionalJmpAddress -> [Instr]
jgt a = [IInstr $ IJGreater a]

jge :: ConditionalJmpAddress -> [Instr]
jge a = [IInstr $ IJGreaterEq a]

tax :: [Instr]
tax = [IInstr ISwapAX]

txa :: [Instr]
txa = [IInstr ISwapXA]

ret :: Literal -> [Instr]
ret a = [IInstr $ IRet (ReturnAddrLiteral a)]

retA :: [Instr]
retA = [IInstr $ IRet (ReturnAddrA A)]

-- |The stream of emitted instructions will consist of both "true" BPF instructions and labels
data Instr = IInstr BPFInstr | ILabel Label

goto :: String -> Label
goto s = Label s

label :: String -> [Instr]
label s = [ILabel (Label s)]

-- | Pretty print 'BPFInstr' instructions. Not exported, just used to implements 'toString'
-- | in the 'StringRep' instance of 'BPFInstr'
printBPFInstr :: BPFInstr -> String
printBPFInstr (ILoad reg typ addr) =
  let location = case reg of
                   A -> "ld"
                   X -> "ldx"
      rest = toString addr
      prefix = case typ of
                (LoadWord) -> ""
                (LoadHalfWord) -> "h"
                (LoadByte) -> "b"
  in location ++ prefix ++ " " ++ rest
printBPFInstr (IStore namedReg addr) =
  let location = case namedReg of
                   A -> "st"
                   X -> "stx"
      rest = toString addr
  in location ++ " " ++ rest
printBPFInstr (ILeftShift a)  = "lsh " ++ toString a
printBPFInstr (IRightShift a) = "rsh " ++ toString a
printBPFInstr (IAnd a)        = "and " ++ toString a
printBPFInstr (IOr a)         = "or "  ++ toString a
printBPFInstr (IMod a)        = "mod " ++ toString a
printBPFInstr (IMult a)       = "mul " ++ toString a
printBPFInstr (IDiv a)        = "div " ++ toString a
printBPFInstr (IAdd a)        = "add " ++ toString a
printBPFInstr (ISub a)        = "sub " ++ toString a
printBPFInstr (IJmp a)        = "jmp " ++ toString a
printBPFInstr (IJAbove a)     = "ja "  ++ toString a
printBPFInstr (IJEq a)        = "jeq " ++ toString a
printBPFInstr (IJNotEq a)     = "jne " ++ toString a
printBPFInstr (IJGreater a)   = "jgt " ++ toString a
printBPFInstr (IJGreaterEq a) = "jge " ++ toString a
printBPFInstr (ISwapAX)       = "tax"
printBPFInstr (ISwapXA)       = "txa"
printBPFInstr (IRet a)        = "ret " ++ toString a

--
-- * Below are all of the 'StringRep' instances needed to serialize a representation of BPF assembly
-- * instructions for outputting.
--

-- | A class that creates a 'toString' method
class StringRep a where
  toString :: a -> String

instance StringRep BPFInstr where
  toString = printBPFInstr

-- |Converts a list of BPF assembly instructions and labels into a string representation
instance StringRep [Instr] where
  toString = unlines . map toString

instance StringRep ReturnAddress where
  toString (ReturnAddrLiteral l) = "#" ++ (show . unLiteral) l
  toString (ReturnAddrA namedReg) = --should error if namedReg is X
                                  "a"

instance StringRep IndexedRegister where
  toString (M0) = "M[0]"
  toString (M1) = "M[1]"
  toString (M2) = "M[2]"
  toString (M3) = "M[3]"
  toString (M4) = "M[4]"
  toString (M5) = "M[5]"
  toString (M6) = "M[6]"
  toString (M7) = "M[7]"
  toString (M8) = "M[8]"
  toString (M9) = "M[9]"
  toString (M10) = "M[10]"
  toString (M11) = "M[11]"
  toString (M12) = "M[12]"
  toString (M13) = "M[13]"
  toString (M14) = "M[14]"
  toString (M15) = "M[15]"

instance StringRep LoadAddress where
  toString = let printLoadAddress :: LoadAddress -> String
                 printLoadAddress (LoadAddrLiteral lit) = "#" ++ (show . unLiteral) lit
                 printLoadAddress (LoadAddrByteOff bos) = "[" ++ (show . unOffset) bos ++ "]"
                 printLoadAddress (LoadAddrIndexedRegister m) = toString m
                 printLoadAddress (LoadAddrNamedRegister r) = case r of { A -> "a" ; _ -> "x" }
                 printLoadAddress (LoadAddrNibble lan) = "4*([" ++ (show . unOffset) lan ++ "]&0xf)"
                 printLoadAddress (LoadAddrOffFromX ofx) = "[x + " ++ (show . unOffset) ofx ++ "]"
             in printLoadAddress

instance StringRep StoreAddress where
  toString (StoreAddrIndexedRegister m) = toString m

instance StringRep ArithAddress where
  toString (ArithAddrLiteral lit) = "#" ++ (show . unLiteral) lit
  toString (ArithAddrRegister A) = "A"
  toString (ArithAddrRegister X) = "X"

instance StringRep UnconditionalJmpAddress where
  toString (UnconditionalJmpAddrLabel l) = (unLabel) l ++ ":"

instance StringRep ConditionalJmpAddress where
  toString (ConditionalJmpAddrLiteralTrue lit lt) = "#" ++ (show . unLiteral) lit ++ "," ++ unLabel lt
  toString (ConditionalJmpAddrLiteralTrueFalse lit lt lf) = "#" ++ (show . unLiteral) lit ++ "," ++ unLabel lt ++ "," ++ unLabel lf

instance StringRep Label where
  toString (Label l) = l ++ ":"

instance StringRep Instr where
  toString (IInstr i) = toString i
  toString (ILabel l) = toString l

