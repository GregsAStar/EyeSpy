module Game
    ( beginGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import SizedPicture

beginGame :: IO ()
beginGame = do
    let fps = 60
    let window = InWindow "Eye Spy" (640, 480) (0, 0)
    initialWorld <- genInitialWorld
    playIO window black fps initialWorld draw eventHandler (\_ -> return . id)
    

type Riddle = [(String, Bool)] --[(Riddle_Word, Found_State)]

data GameState = Menu [String] | Board SizedPicture SizedPicture Riddle --Main_Image Text_Background Riddle


genInitialWorld :: IO GameState
genInitialWorld = do
    mainImageBMP <- readSizedPicture "./assets/placeholder.bmp" --For test purposes, will be changed later
    textBgBMP <- readSizedPicture "./assets/placeholder.bmp" --For test purposes, will be changed later
    let mainImage = modSizedPicture (scale 1.0 0.5) mainImageBMP
    let textBg = modSizedPicture (translate 0 240 . scale 1.0 0.5) textBgBMP
    let riddle = [("Item 1", False), ("Item 2", False), ("Item 3", False)] --For test purposes, will be changed later
    return $ Board mainImage textBg riddle

draw :: GameState -> IO Picture
draw state
    |Menu options <- state
    = return Blank
    |Board mainImage textBG riddle <- state
    = do
        return $ sizedPictures (mainImage:textBG:[])

eventHandler :: Event -> GameState -> IO GameState
eventHandler _ state = return state
