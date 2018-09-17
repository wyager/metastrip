module Main (main) where

import qualified Options.Applicative as OA
import           Control.Applicative (some, (<|>), (<**>))

import qualified Codec.Picture as Pic
import Lib(Aggressiveness(Normal,High),fastRandom,clean')

import           Crypto.Random (getRandomBytes)
import qualified Data.ByteArray.Encoding as BE
import qualified Data.ByteString.Char8 as B8

import qualified GHC.Conc as Conc
import           Control.Concurrent.Async (forConcurrently_)
import           Data.Pool (createPool, withResource)

import qualified System.FilePath.Posix as Path
import           Data.Char (toUpper)


-- How to name the output files
data Naming = Random | SameName | BeforeExtension String deriving Show

-- Where to put them
data Directory = Same | Different FilePath deriving Show

-- How much to randomize the pixels
data Randomization = NoRandomization | RandomizeWith Aggressiveness deriving Show

-- How many threads to do it on
data NumThreads = AutoSelectNumThreads | SpecifiedNumThreads Int deriving Show

data Command 
    = Clean Naming Directory Randomization Int NumThreads [FilePath] 
    deriving Show

command :: OA.Parser Command
command = Clean <$> naming <*> directory <*> randomization <*> jpegSaveQuality <*> numThreads <*> filepaths
    where 
    filepaths = some $ OA.argument OA.str $ OA.metavar "FILES..."
    jpegSaveQuality = OA.option OA.auto 
        (OA.long "jpeg-quality"  
      <> OA.short 'j'  
      <> OA.help "JPEG save quality (1-100)" 
      <> OA.showDefault 
      <> OA.value 50)
    directory = OA.option (OA.maybeReader $ Just . Different) 
        (OA.long "output-dir" 
      <> OA.short 'o' 
      <> OA.help "Output directory (default: Same directory)" 
      <> OA.value Same 
      <> OA.metavar "DIR")
    numThreads = specifiedNumThreads <|> pure AutoSelectNumThreads
        where specifiedNumThreads = SpecifiedNumThreads <$> OA.option OA.auto 
                                        (OA.long "num-threads" 
                                      <> OA.short 'n' 
                                      <> OA.help "Specify number of threads (default: # of cores available)" 
                                      <> OA.metavar "INT")
    randomization = (RandomizeWith <$> aggressiveness) <|> none
        where
        aggressiveness = OA.flag Normal High $ 
                            OA.long "aggressive" 
                         <> OA.short 'a' 
                         <> OA.help "Aggressively randomize pixel values (default: Slight randomization)"
        none = OA.flag' NoRandomization $ 
                            OA.long "dont-randomize" 
                         <> OA.short 'd' 
                         <> OA.help "Do not randomize pixel values at all"
    naming = random <|> inPlace <|> beforeExtension
        where
        random = OA.flag' Random (OA.long "random-name" 
                               <> OA.short 'r'
                               <> OA.help "Write output files with a random name")
        inPlace = OA.flag' SameName (OA.long "same-name" 
                                  <> OA.short 's' 
                                  <> OA.help "Write output files with the same name as the input files, overwriting if necessary")
        beforeExtension = BeforeExtension <$> OA.strOption 
                                    (OA.long "before-extension" 
                                  <> OA.short 'b' 
                                  <> OA.metavar "PREFIX" 
                                  <> OA.value "scrubbed"
                                  <> OA.showDefault
                                  <> OA.help "Output a scrubbed copy of a.jpg at a.PREFIX.jpg") 

getCommand :: IO Command
getCommand = OA.customExecParser 
                (OA.prefs (OA.showHelpOnError <> OA.showHelpOnEmpty))
                (OA.info (command <**> OA.helper) OA.fullDesc)

cleanFile :: Naming -> Directory -> Randomization -> Int -> FilePath -> IO (Either String ())
cleanFile naming directory rand jpegQuality path = case map toUpper ext of
        ".PNG" -> go Pic.savePngImage
        ".JPG" -> go (Pic.saveJpgImage jpegQuality)
        ".JPEG" -> go (Pic.saveJpgImage jpegQuality)
        ".BMP" -> go Pic.saveBmpImage
        ".TIFF" -> go Pic.saveTiffImage
        _ -> return $ Left $ "Unsupported extension " ++ ext ++ " for file " ++ path
    where
    ext = Path.takeExtension path 
    basename = Path.takeBaseName path
    directoryPath = case directory of
        Same -> Path.takeDirectory path
        Different dir -> dir
    outPath = case naming of
        SameName -> return (directoryPath Path.</> basename ++ ext)
        BeforeExtension str -> return $ directoryPath Path.</> basename ++ ('.':str) ++ ext
        Random -> do
            randomBytes :: B8.ByteString <- getRandomBytes 4
            let filename = B8.unpack $ BE.convertToBase BE.Base32 randomBytes
            return (directoryPath Path.</> filename ++ ext)
    go formatter = 
        Pic.readImage path >>= \case
            Left err -> return (Left $ "Error with image " ++ path ++ ": " ++ err)
            Right image -> Right <$> do
                image' <- case rand of
                    NoRandomization -> return image
                    RandomizeWith aggr -> fastRandom $ clean' aggr image
                thePath <- outPath
                formatter thePath image' 

main :: IO ()
main = do
    Clean naming dir rand jpegQuality numThreads paths <- getCommand
    numProcessors <- Conc.getNumProcessors
    Conc.setNumCapabilities numProcessors
    let numThreads' = case numThreads of
            AutoSelectNumThreads -> numProcessors
            SpecifiedNumThreads n -> n
    threadPool <- createPool (return ()) (const $ return ()) 1 1.0 numThreads'
    forConcurrently_ paths $ \path -> 
        withResource threadPool $ \() -> do 
            result <- cleanFile naming dir rand jpegQuality path
            either print return result
