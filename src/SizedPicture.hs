
module SizedPicture(
    SizedPicture,
    SizedPictures,
    sizedPicture,
    modSizedPicture,
    sizedPictures,
    readPicture,
    readSizedPicture
    ) where

import Graphics.Gloss
import Codec.BMP(BMP, readBMP, Error)
import System.IO(FilePath)

type SizedPicture = (Picture, (Int, Int))

type SizedPictures = [SizedPicture] 

sizedPicture :: SizedPicture -> Picture
sizedPicture pic = fst pic

sizedPictures :: SizedPictures -> Picture
sizedPictures pics = pictures $ map (sizedPicture) pics

modSizedPicture :: (Picture -> Picture) -> SizedPicture -> SizedPicture
modSizedPicture mod pic = (mod $ fst pic, snd pic)

readBitmapData :: FilePath -> IO (Maybe BitmapData)
readBitmapData path = do
    bmp <- readBMP path
    either 
        (\x -> do
            putStrLn $ show x
            return Nothing
        ) 
        (\x -> return $ Just $ bitmapDataOfBMP x)
        bmp

readPicture :: FilePath -> IO Picture
readPicture path = do
    bmpdata <- readBitmapData path
    return $ maybe Blank bitmap bmpdata

readSizedPicture :: FilePath -> IO SizedPicture
readSizedPicture path = do
    bmpdata <- readBitmapData path
    let pic = maybe Blank bitmap bmpdata
    let size = maybe (0, 0) bitmapSize bmpdata
    return (pic, size)