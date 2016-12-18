module ColorTheme (myTheme) where

import Data.Bits
import Data.Monoid

import Yi

-- Color theme roughly based on emacs tango dark.
myTheme :: Bool -> Theme
myTheme usePango = defaultTheme `override` \sets _ -> sets 
    { baseAttributes = emptyAttributes
        { background = rgb $ if usePango then 0x222222 else 0x000000
        , foreground = rgb 0xE0E2E4
        }
    , selectedStyle = withBg (rgb 0x204a87)
    , variableStyle = withFg (rgb 0xC0C2C4)
    , errorStyle = withFg (rgb 0xff4b4b) <> withBg (rgb 0x440000)
    , commentStyle = withFg (rgb 0x66747B)
    , keywordStyle = withFg (rgb 0xb4fa70)
    , numberStyle = withFg (rgb 0xFFCD22)
    , preprocessorStyle = withFg (rgb 0x73d216)
    , stringStyle = withFg (rgb 0xe9b96e)
    , typeStyle = withFg (rgb 0x8cc4ff)
    , dataConstructorStyle = withFg (rgb 0x8cc4ff)
    , importStyle = withFg (rgb 0xb4fa70)
    , operatorStyle = withFg (rgb 0xb4fa70)
    , builtinStyle = withFg (rgb 0xe090d7)
    }

rgb :: Int -> Color
rgb val = RGB
    (fromIntegral $ (val `shiftR` 16) .&. 0xFF)
    (fromIntegral $ (val `shiftR`  8) .&. 0xFF)
    (fromIntegral $ (val .&. 0xFF))
