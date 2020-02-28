{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.ST
import           Data.List.Split
import           Data.Maybe
import qualified Data.Sequence           as Seq
import           Data.Tuple.Select
import qualified Data.Vector.Storable    as VS
import           Data.Word               (Word8)
import           Options.Applicative
import           System.FilePath.Lens


main :: IO ()
main = do
  cli@CLI {..} <- execParser cliParser
  src <- either (error "Couldn't read image") convertRGBA8 <$> readImage cImgPath
  let new = makeImage cShift cChannelSrc cChannelTgt src
  writePng (makeFileName cli cImgPath) new
  where
    makeFileName CLI {..} imgPath =
      let baseDir     = imgPath ^. directory
          [name, ext] = case splitOn "." $ imgPath ^. filename of
            (n:x:_) -> [n, x]
            _       -> error "Invalid filename/extension."
      in baseDir <> "/" <> name <> "-shifted-" <> show cChannelSrc
          <> "-" <> show cChannelTgt <> "-"
          <> show cShift <> "." <> ext

    cliParser = info (helper <*> parseCLI) (header "color-shift")

    parseCLI = CLI
      <$> strOption (long "file" <> help "Image path")
      <*> option auto (long "shift" <> help "By how many pixels to shift the source channel")
      <*> option channelParser (long "src-channel")
      <*> option channelParser (long "tgt-channel")

channelParser :: ReadM Channel
channelParser = str >>= \s -> case s of
  "red" -> pure Red
  "green" -> pure Green
  "blue" -> pure Blue
  _ -> readerError "Permitted inputs are \"red\", \"green\", and \"blue\"."

-- | CLI.
data CLI = CLI
  { cImgPath    :: FilePath -- ^ Path to the file to process.
  , cShift      :: Int -- ^ By which amount to shift
  , cChannelSrc :: Channel -- ^ Source channel
  , cChannelTgt :: Channel -- ^ Target channel
  } deriving (Eq, Show)

makeImage
  :: Int -- ^ By how much to shift
  -> Channel
  -> Channel
  -> Image PixelRGBA8 -- ^ Source image.
  -> Image PixelRGBA8
makeImage
  = shiftChannel

-- | Indexed from (0 0) - top left to (imgWidth-1 imgHeight-1) - bottom right
data Rectangle = Rectangle
  { x1 :: Int
  , y1 :: Int
  , x2 :: Int
  , y2 :: Int
} deriving (Show)

-- |
shiftChannel :: Int -> Channel -> Channel -> Image PixelRGBA8 -> Image PixelRGBA8
shiftChannel a ch1 ch2 src@Image {..} = runST $ do
  mimg <- unsafeThawImage src
  void $ writeRectangle mimg np (Rectangle 0 0 (imageWidth-1) (imageHeight-1))
  unsafeFreezeImage mimg
  where np = zipChannels a ch1 ch2 imageData

zipChannels :: Int -> Channel -> Channel -> VS.Vector Word8 -> Seq.Seq PixelRGBA8
zipChannels a ch1 ch2 id =
  case (ch1, ch2) of
    (Red, Red)     -> fmap makePixel (Seq.zip4 (sh r) g b al)
    (Red, Green)   -> fmap makePixel (Seq.zip4 r (sh r) b al)
    (Red, Blue)    -> fmap makePixel (Seq.zip4 r g (sh r) al)
    (Green, Green) -> fmap makePixel (Seq.zip4 r (sh g) b al)
    (Green, Red)   -> fmap makePixel (Seq.zip4 (sh g) g b al)
    (Green, Blue)  -> fmap makePixel (Seq.zip4 r g (sh g) al)
    (Blue, Blue)   -> fmap makePixel (Seq.zip4 r g (sh b) al)
    (Blue, Red)    -> fmap makePixel (Seq.zip4 (sh b) g b al)
    (Blue, Green)  -> fmap makePixel (Seq.zip4 r (sh b) b al)
  where makePixel d = PixelRGBA8 (sel1 d) (sel2 d) (sel3 d) (sel4 d)
        r = extractRed id
        g = extractGreen id
        b = extractBlue id
        al = extractAlpha id
        sh sq = (Seq.drop a sq) Seq.>< (Seq.take a sq)

-- | Which channel are we shifting
data Channel = Red | Green | Blue deriving (Show, Eq)

extractRed :: VS.Vector Word8 -> Seq.Seq Word8
extractRed = extractChannel 0

extractGreen :: VS.Vector Word8 -> Seq.Seq Word8
extractGreen = extractChannel 1

extractBlue :: VS.Vector Word8 -> Seq.Seq Word8
extractBlue = extractChannel 2

extractAlpha :: VS.Vector Word8 -> Seq.Seq Word8
extractAlpha = extractChannel 3

extractChannel :: Int -> VS.Vector Word8 -> Seq.Seq Word8
extractChannel = go Seq.empty
  where
    go !acc !a !d
      | VS.length d == 0    = acc
      | otherwise =
        go (acc Seq.|> makePixel a (VS.take 4 d)) a (VS.drop 4 d)

    makePixel a d = d VS.! a


-- | Write vc vector of pixels of Rectangle rt dimensions into mutable img mmg
writeRectangle :: (Control.Monad.Primitive.PrimMonad m, Pixel a)
  => MutableImage (Control.Monad.Primitive.PrimState m) a -> Seq.Seq a -> Rectangle -> m (Image a)
writeRectangle mmg vc rt =
  writeRow (x1 rt) (x2 rt) (x1 rt) (y1 rt) (y2 rt) vc mmg
  where
    writeRow c c' ic r r' v mimg
        | Seq.length v == 0 = unsafeFreezeImage mimg
        | c > c' = writeRow ic c' ic (r + 1) r' (Seq.drop (c'-ic+1) v) mimg
        | otherwise = do
            writePixel mimg c r (Seq.index v (c - ic))
            writeRow (c + 1) c' ic r r' v mimg

