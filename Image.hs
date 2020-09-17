module Image where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Builder as B
import Data.Word
import System.IO

data Color = Color {red, green, blue :: Word16} deriving Show

-- convert (r,g,b) Double (0->1) to a Word32 Color.
unitColor :: Double -> Double -> Double -> Color
unitColor r g b = Color (fromIntegral $ round $ r * 65535)
                        (fromIntegral $ round $ g * 65535)
                        (fromIntegral $ round $ b * 65535)

-- make a Farbfeld image. image format:
--     "farbfeld" magic bytes
--     uint32_t width, height
--     uint16_t r, g, b, a
-- for a given file path and resolution, call the
-- given function for each (x,y) pixel and save the
-- Color to the file.
makeImage :: FilePath -> (Word32, Word32) -> ((Word32, Word32) -> Color) -> IO ()
makeImage path (w,h) doPix =
    withFile path WriteMode $ \hnd -> do
        hSetBinaryMode hnd True
        hSetBuffering hnd (BlockBuffering Nothing)
        B.hPutBuilder hnd $
            B.byteString (C8.pack "farbfeld") <>
            B.word32BE w <>
            B.word32BE h

        let indices = flip (,) <$> [0..h-1] <*> [0..w-1]
            writePix (Color r g b) =
                B.word16BE r <>
                B.word16BE g <>
                B.word16BE b <>
                B.word16BE 65535

        B.hPutBuilder hnd $ mconcat $ writePix . doPix <$> indices
