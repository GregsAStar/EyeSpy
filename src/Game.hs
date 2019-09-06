module Game
    ( beginGame
    ) where

import Control.Monad

import Data.Text hiding (map)
import Foreign.C.Types

import SDL
import SDL.Vect
--import Paths_sdl(getDataFileName)


screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

screenSize = V2 screenWidth screenHeight

beginGame :: IO ()
beginGame = do
    initializeAll
    HintRenderScaleQuality $= ScaleLinear
    window <- createWindow (pack "Eye Spy") defaultWindow {windowInitialSize = screenSize}
    renderer <- createRenderer window (-1) defaultRenderer
    gameLoop renderer

gameLoop :: Renderer -> IO ()
gameLoop renderer = do
    events <- pollEvents

    unless (QuitEvent `elem` (map eventPayload events)) (gameLoop renderer)