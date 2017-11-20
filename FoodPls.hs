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
-- http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataday?restaurantid=33

getLunchUrl :: Integer -> String
getLunchUrl d = "http://carboncloudrestaurantapi.azurewebsites.net/api/menuscreen/getdataday?restaurantid=" ++ show d


getLunch :: String -> IO String
getLunch url = do
  rsp <- Network.HTTP.simpleHTTP (getRequest url)
  getResponseBody rsp

getLunchFromId :: String -> Integer -> IO Lunch
getLunchFromId n i = do
  body <- getLunch (getLunchUrl i)
  return (n, decode (pack body) :: Maybe LunchMenu)



lunches = do
  let providers = [("Linsen", 33),("Kårrestaurangen", 5), ("L's kitchen", 8), ("Express", 7), ("L's Resto", 32), ("Kokboken", 35)]
  ls <- sequence [getLunchFromId n i | (n, i) <-providers]
  printer <- byteStringMakerFromEnvironment
  mapM_ BS.putStr . chunksToByteStrings printer $ printLunches ls


printLunches :: [Lunch] -> [Chunk String]
printLunches []     = []
printLunches (l:ls) = printLunch l ++ printLunches ls

printLunch (name, Just menu) = concat ([underline (bold (chunk (name ++ "\n")) & fore blue)]:[[r | r<-printRecipeCategory rc] | rc<-recipeCategories menu, printRecipeCategory rc /= []])  ++ [chunk "\n"]

printRecipeCategory :: RecipeCategory -> [Chunk String]
printRecipeCategory rc = concat [[bold (chunk (name rc ++ ": ")), chunk (printRecipe recipe)] | recipe<-recipes rc, printRecipe recipe /= ""]

printRecipe :: Recipe -> String
printRecipe r = printDisplayName (head (displayNames r))

printDisplayName :: DisplayName -> String
printDisplayName d = (displayName d ++ "\n")


type Lunch = (String, Maybe LunchMenu)


data DisplayName = DisplayName {
  displayName :: String
} deriving (Show, Generic)
instance FromJSON DisplayName

data Recipe = Recipe {
  price        :: Int,
  displayNames :: [DisplayName]
} deriving (Show, Generic)
instance FromJSON Recipe

data RecipeCategory = RecipeCategory {
  name    :: String,
  recipes :: [Recipe]
} deriving (Show, Generic)
instance FromJSON RecipeCategory

data LunchMenu = LunchMenu {
  menuDate         :: String,
  recipeCategories :: [RecipeCategory]
} deriving (Generic, Show)

instance FromJSON LunchMenu


main = lunches
