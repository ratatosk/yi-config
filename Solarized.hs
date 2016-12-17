module Solarized (solarizedTheme) where

import Yi
import Yi.Style
import Yi.Style.Library

-- Solarized theme borrowed from @jaredloomis
solarizedTheme :: Theme
solarizedTheme = defaultTheme `override` \sets _ -> sets {
    baseAttributes = emptyAttributes{
        background = solarizedBase03,
        foreground = solarizedRed
        },
    selectedStyle = withBg grey,
    variableStyle = withFg solarizedBlue,
    errorStyle = withFg solarizedOrange,
    commentStyle = withFg grey,
    keywordStyle = withFg solarizedCyan,
    numberStyle = withFg solarizedCyan,
    preprocessorStyle = withFg grey,
    stringStyle = withFg grey,
    typeStyle = withFg solarizedYellow,
    dataConstructorStyle = withFg solarizedYellow,
    importStyle = withFg solarizedCyan,
    operatorStyle = withFg solarizedCyan,
    quoteStyle = withFg solarizedOrange,
    builtinStyle = withFg solarizedRed
}

solarizedBase03 :: Color
solarizedBase03 = RGB 0 43 54

solarizedBase02 :: Color
solarizedBase02 = RGB 7 54 66

solarizedBase01 :: Color
solarizedBase01 = RGB 88 110 117

solarizedBase00 :: Color
solarizedBase00 = RGB 101 123 131

solarizedBase0 :: Color
solarizedBase0 = RGB 131 148 150

solarizedBase1 :: Color
solarizedBase1 = RGB 147 161 161

solarizedBase2 :: Color
solarizedBase2 = RGB 238 232 213

solarizedBase3 :: Color
solarizedBase3 = RGB 253 246 227

solarizedYellow :: Color
solarizedYellow = RGB 181 137 0

solarizedOrange :: Color
solarizedOrange = RGB 203 75 22

solarizedRed :: Color
solarizedRed = RGB 220 50 47

solarizedMagenta :: Color
solarizedMagenta = RGB 211 54 130

solarizedViolet :: Color
solarizedViolet = RGB 108 113 196

solarizedBlue :: Color
solarizedBlue = RGB 38 139 210

solarizedCyan :: Color
solarizedCyan = RGB 42 161 152

solarizedGreen :: Color
solarizedGreen = RGB 133 153 0