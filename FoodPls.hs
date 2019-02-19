{-# LANGUAGE DeriveGeneric #-}
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 hiding (concat, head, putStr,
                                             unlines)
import           GHC.Generics
import           Network.HTTP
import           Prelude                    hiding (putStr)
import           Rainbow
import           Test.QuickCheck

lunchIds = [
  ("Linsen", "b672efaf-032a-4bb8-d2a5-08d558129279"),
  ("Kårrestaurangen", "21f31565-5c2b-4b47-d2a1-08d558129279"),
  ("Express", "3d519481-1667-4cad-d2a3-08d558129279"),
  ("Hyllan", "a7f0f75b-c1cb-4fc3-d2a6-08d558129279"),
  ("S.M.A.K", "3ac68e11-bcee-425e-d2a8-08d558129279"),
  ("L's kitchen", "c74da2cf-aa1a-4d3a-9ba6-08d5569587a1"),
  -- ("L's Resto", "c6742862-3cc5-47b1-d2a4-08d558129279"), -- Output gets quite long with these
  -- ("L's Express", "871c63d7-4ddb-46b8-d2a0-08d558129279"),
  ("Kokboken", "c74da2cf-aa1a-4d3a-9ba6-08d5569587a1")] :: [(String, String)]

getLunchUrl :: String -> String
getLunchUrl i = "http://carbonateapiprod.azurewebsites.net/api/v1/mealprovidingunits/" ++ i ++ "/dishoccurrences"


getLunch :: String -> IO String
getLunch url = do
  rsp <- Network.HTTP.simpleHTTP (getRequest url)
  getResponseBody rsp

getLunchFromId :: String -> String -> IO Lunch
getLunchFromId n i = do
  body <- getLunch (getLunchUrl i)
  return (n, decode (pack body) :: Maybe [Recipe])



lunches = do
  ls <- sequence [getLunchFromId n i | (n, i) <- lunchIds]
  printer <- byteStringMakerFromEnvironment
  mapM_ BS.putStr . chunksToByteStrings printer $ printLunches ls


printLunches :: [Lunch] -> [Chunk String]
printLunches []     = []
printLunches (l:ls) = printLunch l ++ printLunches ls

printLunch :: ([Char], Maybe [Recipe]) -> [Chunk [Char]]
printLunch (name, Just recepies) = concat ([underline (bold (chunk (name ++ "\n")) & fore blue)]:[printRecipe r | r <- recepies])  ++ [chunk "\n"]
printLunch (name, Nothing) = [underline (bold (chunk (name ++ "\n\n")) & fore blue)]

printRecipe :: Recipe -> [Chunk String]
printRecipe rc = [bold (chunk (dishTypeName (dishType rc) ++ ": ")), chunk (printDisplayName (head (displayNames rc)))]

printDisplayName :: DisplayName -> String
printDisplayName d = (dishDisplayName d ++ "\n")


type Lunch = (String, Maybe [Recipe])


data DisplayName = DisplayName {
  dishDisplayName :: String
} deriving (Show, Generic)
instance FromJSON DisplayName

data DishType = DishType {
  dishTypeName :: String
} deriving (Show, Generic)
instance FromJSON DishType

data Recipe = Recipe {
  dishType      :: DishType,
  displayNames  :: [DisplayName]
} deriving (Generic, Show)
instance FromJSON Recipe


main = lunches
