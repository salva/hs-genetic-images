module Render where

import Codec.Picture (PixelRGBA8 ( .. ), PixelRGB8 (..),  writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

size = 200

colorToPixel (Color { .. }) = PixelRGB8 (floor red * 255) (floor green * 255) (floor blue * 255) 0xff

cToV2 (x :+ y) = V2 (floor x * size) (floor y * size)

imageOfCirclesToDrawing (Circle {..} : cs) = do
  withTexture (uniformTexture $ colorToPixel color) $ fill $ circle (cToV2 center) (floor size * radius)
  imageOfCirclesToDrawing cs

drawSomething :: IO ()
drawSomething = do
  let white = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0 0x86 0xc1 0xff
      recColor = PixelRGBA8 0xFF 0x53 0x73 0xff
      img = renderDrawing 400 400 white $ do
        withTexture (uniformTexture drawColor) $ fill $ circle (V2 0 0) 30
        withTexture (uniformTexture recColor) $ fill $ circle (V2 40 40) 20
--            withTexture (uniformTexture recColor) $ do
--
--            stroke 4 JoinRound (CapRound, CapRound) $
--                   circle (V2 400 200) 40
--            withTexture (uniformTexture recColor) $ do
--              fill $ rectangle (V2 100 100) 200 100

  writePng "yourimage.png" img
