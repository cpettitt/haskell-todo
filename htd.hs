-----------------------------------------------------------------------------
-- |
-- Copyright   :  Copyright (c) 2010 Chris Pettitt
-- License     :  MIT
-- Maintainer  :  cpettitt@gmail.com
--
-- A simple utility for managing tasks.
--
-----------------------------------------------------------------------------

module Main (main) where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory)
import System.Environment (getArgs)
import System.IO (IOMode(..), hGetContents, hPutStr, withFile)

import Text.Printf (printf)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Id = Int
type Todo = String
type Tag = String
type TodoDB = [Todo]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    args <- getArgs

    if null args
        then do
            putStrLn "Haskell Todo (HTD) - Interactive Mode"
            runInputT defaultSettings interactive
        else cmd args

{-------------------------------------------------------------------------------
  Interactive Loop
-------------------------------------------------------------------------------}

interactive :: InputT IO ()
interactive = do
    line <- getInputLine "> "
    case line of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> liftIO (cmd (words input)) >> interactive

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

cmd :: [String] -> IO ()
cmd ("add":desc) =
    modifyDB (add $ unwords desc)

cmd ("rm":idStr:[]) =
    modifyDB (delete $ read idStr)

cmd ("addtag":idStr:tag:[]) =
    modifyDB (adjustTodo (read idStr) (addTag tag))

cmd ("rmtag":idStr:tag:[]) =
    modifyDB (adjustTodo (read idStr) (deleteTag tag))

cmd ("done":idStr:[]) = do
    date <- dateStr
    modifyDB (adjustTodo (read idStr) (addTag $ "@done{" ++ date ++ "}"))

cmd ("undone":idStr:[]) =
    modifyDB (adjustTodo (read idStr) (deleteTag "@done"))

cmd ("ls":tags) =
    withDB (list (Set.fromList tags) (Set.singleton "@done")) >>= putStr

cmd ("lsdone":tags) =
    withDB (list (Set.fromList ("@done":tags)) Set.empty) >>= putStr

cmd ("lsall":tags) =
    withDB (list (Set.fromList tags) Set.empty) >>= putStr

cmd unknown =
    putStrLn $ "Unknown command: " ++ intercalate " " unknown

{-------------------------------------------------------------------------------
  TodoDB - Load / Save
-------------------------------------------------------------------------------}

load :: IO TodoDB
load = do
    dbFile <- getDBFileName
    maybeCreateDB
    withFile dbFile ReadMode $ \h -> do
        c <- hGetContents h
        return $! lines c

save :: TodoDB -> IO ()
save db = do
    dbFile <- getDBFileName
    backupDB
    withFile dbFile WriteMode $ \h -> hPutStr h $ unlines db

{-------------------------------------------------------------------------------
  TodoDB - Pure Manipulation
-------------------------------------------------------------------------------}

empty :: TodoDB
empty = []

add :: String -> TodoDB -> TodoDB
add todo db = db ++ [todo]

delete :: Id -> TodoDB -> TodoDB
delete todoId db = take (todoId - 1) db ++ drop todoId db

adjustTodo :: Id -> (Todo -> Todo) -> TodoDB -> TodoDB
adjustTodo todoId f db
    | todoId > 0 && todoId <= length db =
        take (todoId - 1) db ++ [f (db !! (todoId - 1))] ++ drop todoId db
    | otherwise = db

addTag :: Tag -> Todo -> Todo
addTag tag = (tag ++) . (" " ++)

deleteTag :: Tag -> Todo -> Todo
deleteTag tag = unwords . filter ((/= tag) . stripTagMeta) . words

list :: Set Tag -> Set Tag -> TodoDB -> String
list selTags deselTags db = unlines $ map (uncurry fmtTodo) todos'
    where todos' = filter (filterRule . snd) (todosWithIds db)
          filterRule todo = hasTags selTags todo && noHasTags deselTags todo

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

withDB :: (TodoDB -> a) -> IO a
withDB f = fmap f load

modifyDB :: (TodoDB -> TodoDB) -> IO ()
modifyDB f = withDB f >>= save >> return ()

maybeCreateDB :: IO ()
maybeCreateDB = do
    appDir <- getAppDir
    createDirectoryIfMissing False appDir
    dbExists <- getDBFileName >>= doesFileExist
    unless dbExists (save empty) 

backupDB :: IO ()
backupDB = do
    dbFile <- getDBFileName
    backupFile <- fmap (++ "/.todo.bak") getAppDir
    copyFile dbFile backupFile

getDBFileName :: IO String
getDBFileName = do
    appDir <- getAppDir
    return $ appDir ++ "/todo"

getAppDir :: IO String
getAppDir = getAppUserDataDirectory "htd"

fmtTodo :: Id -> Todo -> String
fmtTodo = printf "[%4d] %s"

-- Returns @True@ if the 'Todo' has all specified 'Tag's.
hasTags :: Set Tag -> Todo -> Bool
hasTags tags todo = tags `Set.isSubsetOf` getTags todo

-- Returns @True@ if the 'Todo' has none of the specified 'Tag's.
noHasTags :: Set Tag -> Todo -> Bool
noHasTags tags todo = Set.null (tags `Set.intersection` getTags todo)

getTags :: Todo -> Set Tag
getTags = Set.fromList . map stripTagMeta . filter isTag . words

stripTagMeta :: Tag -> Tag
stripTagMeta = takeWhile (/= '{')

isTag :: String -> Bool
isTag [] = False
isTag x  = head x `elem` "@:"

todosWithIds :: TodoDB -> [(Id, Todo)]
todosWithIds = zip [1..]

dateStr :: IO String
dateStr = do
    currentTime <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay currentTime
    return $ printf "%d-%d-%d" year month day
