{-# LANGUAGE RankNTypes #-}
module Main where

import System.Environment ( getArgs )
import Codec.Picture (DynamicImage(..), readImage, saveJpgImage, generateImage, PixelRGB8(..), pixelAt)
import System.FilePath (pathSeparators)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (copyFile)
import Codec.Picture.Types (dropTransparency, convertPixel, imageWidth, imageHeight)

data Geom = Geom { topWidth :: Float, bottomWidth :: Float, height :: Float }

dpi :: Float
dpi = 300

cm2pix :: Float -> Float
cm2pix cm = (cm * dpi) / 2.54

imgWidth :: DynamicImage -> Int
imgWidth img =
    case img of
        ImageY8     p -> imageWidth p
        ImageYF     p -> imageWidth p
        ImageYA8    p -> imageWidth p
        ImageRGB8   p -> imageWidth p
        ImageRGBF   p -> imageWidth p
        ImageRGBA8  p -> imageWidth p
        ImageYCbCr8 p -> imageWidth p

imgHeight :: DynamicImage -> Int
imgHeight img =
    case img of
        ImageY8     p -> imageHeight p
        ImageYF     p -> imageHeight p
        ImageYA8    p -> imageHeight p
        ImageRGB8   p -> imageHeight p
        ImageRGBF   p -> imageHeight p
        ImageRGBA8  p -> imageHeight p
        ImageYCbCr8 p -> imageHeight p

applyPixel :: PixelRGB8 -> PixelRGB8
applyPixel = id

convertImage :: DynamicImage -> Geom -> DynamicImage
convertImage fromImg geom = ImageRGB8 $ generateImage handlePixel newWidth newHeight
    where
        tWidth = ceiling $ cm2pix (topWidth geom)
        bWidth = ceiling $ cm2pix (bottomWidth geom)
        newWidth = tWidth
        newHeight = ceiling $ cm2pix (height geom)
        origWidth = imgWidth fromImg
        origHeight = imgHeight fromImg
        getOrig x y =
            case fromImg of
                ImageRGB8 im -> pixelAt im x y
                ImageRGBA8 im -> dropTransparency $ pixelAt im x y
                ImageYCbCr8 im -> convertPixel $ pixelAt im x y
                _ -> error "Unknown format"
        trans :: Int -> Int -> Maybe (Int, Int)
        trans x y =
            let
                baseNewWidth = ceiling $ (fromIntegral ((tWidth * (newHeight - y)) + (bWidth * y)) :: Float) / (fromIntegral newHeight :: Float)
                offset = (newWidth-baseNewWidth) `div` 2
                distX = x-offset
            in if distX < 0 || baseNewWidth <= distX
                then Nothing
                else let
                        tx = round $ (fromIntegral (origWidth * distX) :: Float) / (fromIntegral baseNewWidth :: Float)
                        ty = round $ (fromIntegral (origHeight * y) :: Float) / (fromIntegral newHeight :: Float)
                        p = (tx,ty)
                     in Just p
        handlePixel x y = case trans x y of
                            Nothing -> PixelRGB8 0xff 0xff 0xff
                            Just (nx,ny) ->  getOrig nx ny

handleImage :: String -> Geom -> String -> IO (Either String FilePath)
handleImage from g to = do
    res <- readImage from
    case res of
        Left x -> putStrLn ("Failed to load: " ++ from) >> return (Left x)
        Right img -> do
            putStrLn  $ "Loaded: " ++ from ++ " size: "++show (imgWidth img) ++ "x" ++ show (imgHeight img)
            converted <- return $ convertImage img g
            putStrLn  $ "Converted: " ++ to ++ "size: " ++ show (imgWidth converted) ++  "x" ++ show (imgHeight converted)
            saveJpgImage 95 to converted
            putStrLn "jpg saved"
            return (Right to)

withConvertedFile :: forall a . String -> Geom -> (String -> IO a) -> IO a
withConvertedFile original geom f_act =
    withSystemTempDirectory "tmp"
        (\ outd -> let to=outd++pathSeparators++"out.jpg"
            in handleImage original geom to >> f_act to)

main::IO()
main = do
    from:to:_ <- getArgs
    putStrLn $ "from: " ++ from ++ " to: " ++ to
    withConvertedFile from (Geom 27 17 20) ((`copyFile` to))
    return ()
