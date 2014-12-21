{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Test where

import Control.Monad.Writer
import Data.Maybe
import Data.SafeData

import Data.Map as M

data Person2 = Person2 String Int | Person3 Double deriving (Show)
data Person_v0 = Person_v0 { name_v0 :: String, age_v0 :: Double, friend_v0 :: Person2 } deriving (Show)

data Person_v1 = Person_v1 { name_v1 :: String, age_v1 :: Double, friend_v1 :: Person2 } deriving (Show)

data Person = Person { infos :: M.Map Int String, name :: Maybe String, age :: Double, friends :: [Person2] } deriving (Show)

instance Migrate Person_v1 where
  type MigrateFrom Person_v1 = Person_v0
  migrate (Person_v0 name age friend) = Person_v1 name age friend

instance Migrate Person where
  type MigrateFrom Person = Person_v1
  migrate (Person_v1 name age friend) = Person M.empty (Just name) age [friend]

-- deriveSafeData 2 'base ''Person_v0
-- deriveSafeData 3 'extension ''Person_v1
deriveSafeDataAll ''Person

deriveSafeData 1 'base ''Person2

p :: Person_v0
p = Person_v0 "asdasd" 66 (Person2 "FRUEND" 1)

v = safePut p

p2 :: Person
p2 = safeGet v

v2 = safePut p2

p3 :: Person
p3 = Person (M.fromList [(1, "ONE"), (2, "TWO")]) Nothing 666 [Person2 "ENEMY" 0]
