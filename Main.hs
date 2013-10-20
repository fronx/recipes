module Main where
import Data.List

data Ingredient = Ingredient String deriving (Show, Eq)
data Recipe     = Recipe { recipe_id :: Int
                         , ingredients :: [Ingredient]
                         } deriving Show

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs ys = all (`elem` ys) xs

unique = nub

alsoWith :: [Ingredient] -> [Recipe] -> [Ingredient]
alsoWith given recipes = unique $ concat $ filter (isSubset given) (map ingredients recipes)

recipes = [ Recipe { recipe_id=1, ingredients=[ Ingredient "chicken", Ingredient "rice", Ingredient "tomatoes" ] }
          , Recipe { recipe_id=2, ingredients=[ Ingredient "peppers", Ingredient "rice", Ingredient "tomatoes" ] }
          , Recipe { recipe_id=3, ingredients=[ Ingredient "chicken", Ingredient "fries"] }
          ]

main = do
  print $ alsoWith [ Ingredient "chicken" ] recipes
  print $ alsoWith [ Ingredient "peppers" ] recipes
  print $ alsoWith [ Ingredient "chicken", Ingredient "rice" ] recipes
