module Main

import Data.Vect
import Data.String
import System.REPL


data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (ds : DataStore) -> Vect (size ds) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) item = MkData _ (addItemToEnd items)
  where
    addItemToEnd : Vect origSize String -> Vect (S origSize) String
    addItemToEnd [] = [item]
    addItemToEnd (x :: xs) = x :: addItemToEnd xs

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parse : (input : String) -> Maybe Command
parse input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand cmd (ltrim args)
  where
    parseCommand : String -> String -> Maybe Command
    parseCommand "add" s = Just (Add s)
    parseCommand "get" id = if all isDigit (unpack id) then Just (Get (cast id)) else Nothing
    parseCommand "search" s = Just (Search s)
    parseCommand "size" "" = Just Size
    parseCommand "quit" "" = Just Quit
    parseCommand _ _ = Nothing

getEntry : (pos : Integer) -> (dataStore : DataStore) -> Maybe String
getEntry pos ds =
  case integerToFin pos (size ds) of
    Just id => Just (index id (items ds))
    Nothing => Nothing

search : DataStore -> String -> String
search ds queryStr = resultStringIter "" (items ds) 0
  where
    resultStringIter : (resStr : String) -> Vect n String -> (headId: Integer) -> String
    resultStringIter resStr [] _ = resStr
    resultStringIter resStr (s :: ss) sId =
      if isInfixOf queryStr s then
        let newResStr = resStr ++ show sId ++ ": " ++ s ++ "\n" in
        resultStringIter newResStr ss (sId + 1)
      else
        resultStringIter resStr ss (sId + 1)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds inputStr =
  case parse inputStr of
    Just (Add s) =>
      let output = "ID " ++ show (size ds) ++ "\n"
      in Just (output, addToStore ds s)
    Just (Get pos) =>
      let output = case getEntry pos ds of
            Just entry =>  entry ++ "\n"
            Nothing => "Invalid entry\n"
      in Just(output, ds)
    Just (Search s) => Just (search ds s, ds)
    Just Size =>
      let output = show (size ds) ++ " items are stored.\n"
      in Just (output, ds)
    Nothing => Just ("Invalid command\n", ds)
    Just Quit => Nothing

main : IO()
main = replWith (MkData 0 []) "Command: " processInput
