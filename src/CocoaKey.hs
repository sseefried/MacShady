module CocoaKey (
  KeyCode,  KeyModifier(..), modifiersPressed
) where


import           Data.Int
import           Foreign.C.Types
import           Data.Bits
import           Data.Set (Set)
import qualified Data.Set as S

--
-- Data types to represent Cocoa key presses.
--

data KeyModifier =  CapsLock
                  | Shift
                  | Control
                  | Alt
                  | Command
                  | NumericPad
                  | Help
                  | Function deriving (Show, Ord, Eq)

type KeyCode = Int

maskBit :: KeyModifier -> Int
maskBit km = case km of
  CapsLock   -> 16
  Shift      -> 17
  Control    -> 18
  Alt        -> 19
  Command    -> 20
  NumericPad -> 21
  Help       -> 22
  Function   -> 23

modifiersPressed :: CULong -> Set KeyModifier
modifiersPressed modifiers =
  S.fromList $ filter (testBit modifiers . maskBit)
    [CapsLock, Shift, Control, Alt, Command, NumericPad, Help, Function]