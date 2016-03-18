module HBPF.Filter
  (
    newFilter
  , Filter
  , compile
  , (&&)
  , (||)
  ) where

import Prelude hiding ((&&), (||))

import HBPF.Internal
import Data.Word
import Data.Monoid ((<>))

import qualified Data.List.Utils as U

-- | Restrictions on Filter:
-- | Filters should return their return value in M[15]
-- | Filter's labels should be named label1, label2, etc.. (IN ORDER)
-- | Shouldn't expose the Filter type, people should not be able to combine filters
-- | without fixing the labels
data Filter = Filter {
    toInstr :: [Instr],   -- ^ The list of instructions in the filter
    numLabels :: Int
}

newFilter :: AsmCommand -> Filter
newFilter instrs = Filter instrs (countLabels instrs)

countLabels :: AsmCommand -> Int
countLabels instrs = countTemp instrs 0
                    where
                        countTemp (x:xs) num = case x of
                            ILabel _ -> countTemp xs (num + 1)
                            _        -> countTemp xs num
                        countTemp _ num = num


---- | Load the return value of the previous filter into the A register
loadRetRegister :: AsmCommand
loadRetRegister = ld (LoadAddrIndexedRegister M15) -- assuming each filter's return value is in M[15]

storeRetRegister :: AsmCommand
storeRetRegister = st (StoreAddrIndexedRegister M15)

-- from MissingH package
replaceStr = U.replace

fixLabels :: AsmCommand -> Int -> Int -> AsmCommand
fixLabels instrs numLabels1 startAtLabel = finalInstrs
    where
        finalInstrs = map replacement instrs

        -- all replacers concatenated
        replacement = foldl (.) id replacers

        -- label replacement functions for all labels in instrs
        replacers = map createReplaceFunction [1..numLabels1]

        createReplaceFunction num =
            replace num

        -- a function taking a label number and an instruction,
        -- and if that label is in the instructions, replaces it
        -- with a shifted label
        replace :: Int -> Instr -> Instr
        replace num instr = replaceTemp instr
            where
                oldL = "label" ++ (show num)
                newL = "label" ++ (show (startAtLabel + num))
                replaceTemp (ILabel label) =
                    ILabel $ Label (replaceStr oldL newL (unLabel label))
                replaceTemp (IInstr (IJmp (UnconditionalJmpAddrLabel label))) =
                    (IInstr . IJmp) $ UnconditionalJmpAddrLabel
                                (Label (replaceStr oldL newL (unLabel label)))
                replaceTemp (IInstr (IJAbove (UnconditionalJmpAddrLabel label))) =
                    (IInstr . IJAbove) $ UnconditionalJmpAddrLabel
                                (Label (replaceStr oldL newL (unLabel label)))
                replaceTemp (IInstr (IJEq (condJumpAddr))) =
                    replaceConditionalJmpAddr (IInstr . IJEq) condJumpAddr
                replaceTemp (IInstr (IJNotEq (condJumpAddr))) =
                    replaceConditionalJmpAddr (IInstr . IJNotEq) condJumpAddr
                replaceTemp (IInstr (IJGreater (condJumpAddr))) =
                    replaceConditionalJmpAddr (IInstr . IJGreater) condJumpAddr
                replaceTemp (IInstr (IJGreaterEq (condJumpAddr))) =
                    replaceConditionalJmpAddr (IInstr . IJGreaterEq) condJumpAddr
                replaceTemp other = other
                replaceConditionalJmpAddr constr condJumpAddr =
                    case condJumpAddr of
                        ConditionalJmpAddrLiteralTrue lit label ->
                            constr $
                                ConditionalJmpAddrLiteralTrue lit
                                    (Label ((replaceStr oldL newL (unLabel label))))
                        ConditionalJmpAddrLiteralTrueFalse lit label1 label2 ->
                            constr $
                                ConditionalJmpAddrLiteralTrueFalse lit
                                    (Label (replaceStr oldL newL (unLabel label1)))
                                    (Label (replaceStr oldL newL (unLabel label2)))



-- | Takes an Instr that combines the ret values, stored in A and X registers,
-- | into A register. Note the use of '(<>)' i.e. 'mappend', this syntax is allowed
-- | because all filters are simply lists of 'Instr', and Lists already have a
-- | 'Monoid' instance.
combineRetValues :: [Instr] -> Filter -> Filter -> Filter
combineRetValues instr f g = Filter instrs totalLabels
        where
             totalLabels = (numLabels f) + (numLabels g)
             instrs = toInstr f
                   <> loadRetRegister -- return value stored in M[15]. Move it to M[14].
                   <> st (StoreAddrIndexedRegister M14)
                   <> fixLabels (toInstr g) (numLabels g) (numLabels f)
                   <> loadRetRegister
                   <> ldx (LoadAddrIndexedRegister M14)
                   <> instr
                   <> storeRetRegister

type AsmCommand = [Instr]

epilogue :: AsmCommand
epilogue = ld (LoadAddrIndexedRegister M15) <> retA

addEpilogue :: Filter -> Filter
addEpilogue f = Filter ((toInstr f) ++ epilogue) (numLabels f)

-- | Final compilation step for a filter. Adds the epilogue that sets up the return
-- | value, and returns the set of instructions for the filter.
compile :: Filter -> [Instr]
compile = toInstr . addEpilogue

-- * High-level Combinators
-- Restrictions on using our combinators: Filters must not use M[14] (in addition
-- not using M[15]), so we can temporarily store filter1's return value.

-- | The OR construction, allowing us to build a composed filter that returns
-- | a non-zero exit code iff at least one of the filters does.
(||) :: Filter -> Filter -> Filter
(||) = combineRetValues (orI (ArithAddrRegister X))

-- | The AND construction, allowing us to build a filter returning non-zero status iff
-- | both filters return non-zero statuses, returns 0 otherwise.
(&&) :: Filter -> Filter -> Filter
(&&) = combineRetValues (andI (ArithAddrRegister X))

