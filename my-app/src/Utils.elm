module Utils exposing (Address, defaultAddress, addressFromString, addressToString, addressAdd32Int)

import Word
import Word.Hex
import Char

type alias Address = Word.Word

defaultAddress : Address
defaultAddress = Word.zero Word.Bit64

addressFromString : String -> Address
addressFromString = hexToBytes >> bytesTo64BitInts >> List.head >> Maybe.withDefault defaultAddress

addressToString : Address -> String
addressToString = Word.Hex.fromWord

addressAdd32Int : Int -> Address -> Address
addressAdd32Int x = Word.add (Word.D 0 x)

hexToInt : Char -> Int
hexToInt char =
    let x = Char.toCode char
     in if (x >= 48) && (x <= 57) then x - 48 -- 0 to 9
        else if (x >= 97) && (x <= 102) then x - 87 -- a to f
        else 0 -- treat any other character as 0, so zfzfzfzf == 0f0f0f0f

hexToBytes : String -> List Int
hexToBytes str =
  let list = String.toList str
      go = \chars ->
        case chars of
          h :: l :: rest ->
              (hexToInt h) * 2 ^ 4 + (hexToInt l) :: go rest
          [] -> []
          _  -> []
   in go list

bytesTo64BitInts : List Int -> List Word.Word
bytesTo64BitInts list =
    case list of
        y3 :: y2 :: y1 :: y0 :: x3 :: x2 :: x1 :: x0 :: rest ->
            let msb = (y3 * 2^24 + y2 * 2^16 + y1 * 2^8 + y0)
                lsb = (x3 * 2^24 + x2 * 2^16 + x1 * 2^8 + x0)
                addr = Word.D msb lsb
             in addr :: bytesTo64BitInts rest
        [] -> []
        _  -> []