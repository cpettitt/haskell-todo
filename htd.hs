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

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), Underlining(..), setSGRCode)
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
type CmdHandler = [String] -> IO ()

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
        else dispatch args

{-------------------------------------------------------------------------------
  Interactive Loop
-------------------------------------------------------------------------------}

interactive :: InputT IO ()
interactive = do
    line <- getInputLine "> "
    case line of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> liftIO (dispatch (words input)) >> interactive

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

dispatch :: [String] -> IO ()
dispatch [] = return ()
dispatch (cmd:args) = case lookup cmd cmdsWithoutHelp of
        Nothing -> printError $ "Unknown command: " ++ unwords (cmd:args)
        Just f  -> f args
    where cmdsWithoutHelp = map (\(x,y,_) -> (x,y)) cmds

cmds :: [(String, CmdHandler, String)]
cmds =
    [("add",    cmdAdd,    "adds a task")
    ,("change", cmdChange, "changes a task")
    ,("rm",     cmdRm,     "removes a task")
    ,("addtag", cmdAddTag, "adds a tag to a task")
    ,("rmtag",  cmdRmTag,  "removes a tag from a task")
    ,("done",   cmdDone,   "marks a task as done")
    ,("undone", cmdUndone, "resets the done state for a task")
    ,("projs",  cmdProjs,  "list the project names")
    ,("conts",  cmdConts,  "list the context names")
    ,("ls",     cmdLs,     "list all tasks that are not done")
    ,("lsdone", cmdLsDone, "list all tasks that are done")
    ,("lsall",  cmdLsAll,  "list all tasks")
    ,("help",   cmdHelp,   "this help information")
    ]

cmdAdd :: CmdHandler
cmdAdd []   = printError "Usage: add <description>"
cmdAdd desc = modifyDB (add $ unwords desc)

cmdChange :: CmdHandler
cmdChange (idStr:desc)
    | not (null desc) = modifyDB (adjustTodo (read idStr) (const $ unwords desc))
cmdChange _ = printError "Usage: change <id> <description>"

cmdRm :: CmdHandler
cmdRm (idStr:[]) = modifyDB (delete $ read idStr)
cmdRm _ = printError "Usage: rm <id>"

cmdAddTag :: CmdHandler
cmdAddTag (idStr:tag:[]) = modifyDB (adjustTodo (read idStr) (addTag tag))
cmdAddTag _ = printError "Usage: addtag <id> <tag>"

cmdRmTag :: CmdHandler
cmdRmTag (idStr:tag:[]) = modifyDB (adjustTodo (read idStr) (deleteTag tag))
cmdRmTag _ = printError "Usage: rmtag <id> <tag>"

cmdDone :: CmdHandler
cmdDone (idStr:[]) = do
    date <- dateStr
    modifyDB (adjustTodo (read idStr) (addTag $ "@done{" ++ date ++ "}"))
cmdDone _ = printError "Usage: done <id>"

cmdUndone :: CmdHandler
cmdUndone (idStr:[]) = modifyDB (adjustTodo (read idStr) (deleteTag "@done"))
cmdUndone _ = printError "Usage: undone <id>"

cmdProjs :: CmdHandler
cmdProjs [] = withDB (unlines . map colorize . Set.toList . filterTags isProject) >>= putStr
cmdProjs _ = printError "Usage: projs"

cmdConts :: CmdHandler
cmdConts [] = withDB (unlines . map colorize . Set.toList .  filterTags isContext) >>= putStr
cmdConts _ = printError "Usage: conts"

cmdLs :: CmdHandler
cmdLs tags = withDB (list (Set.fromList tags) (Set.singleton "@done")) >>= putStr

cmdLsDone :: CmdHandler
cmdLsDone tags = withDB (list (Set.fromList ("@done":tags)) Set.empty) >>= putStr

cmdLsAll :: CmdHandler
cmdLsAll tags = withDB (list (Set.fromList tags) Set.empty) >>= putStr

cmdHelp :: CmdHandler
cmdHelp _ = do
        putStrLn "Haskell Todo (HTD) Commands:"
        putStr $ unlines $ map helpInfo cmds
    where
        helpInfo (cmd, _, help) = printf "%-10s %s" cmd help

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
modifyDB f = catch doModify handleError
    where
        doModify = do
            db' <- withDB f
            db' `seq` save db'
        handleError e = print e >> return ()

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
fmtTodo todoId todo = printf "[%4s] %s" idColored todoColored
    where
        idColored = applyColor idColor $ show todoId
        todoColored  = unwords . map colorize . words $ todo

colorize :: String -> String
colorize x
    | isContext x = applyColor contextColor x
    | isProject x = applyColor projectColor x
    | otherwise   = x

-- Color codes
idColor :: [SGR]
idColor      = [SetColor Foreground Vivid Magenta]

resetColor :: [SGR]
resetColor   = []

contextColor :: [SGR]
contextColor = [SetColor Foreground Vivid Green, SetUnderlining SingleUnderline]

projectColor :: [SGR]
projectColor = [SetColor Foreground Vivid Blue, SetUnderlining SingleUnderline]

applyColor :: [SGR] -> String -> String
applyColor sgr str = printf "%s%s%s" (setSGRCode sgr) str (setSGRCode resetColor)

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

isContext :: String -> Bool
isContext [] = False
isContext x  = head x == '@'

isProject :: String -> Bool
isProject [] = False
isProject x  = head x == ':'

isTag :: String -> Bool
isTag x  = isContext x || isProject x

filterTags :: (Tag -> Bool) -> TodoDB -> Set Tag
filterTags f = Set.fromList . map stripTagMeta . filter f . words . unlines

todosWithIds :: TodoDB -> [(Id, Todo)]
todosWithIds = zip [1..]

dateStr :: IO String
dateStr = do
    currentTime <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay currentTime
    return $ printf "%d-%d-%d" year month day

printError :: String -> IO ()
printError x = do
    putStrLn x
    putStrLn "Use 'help' for usage information."
