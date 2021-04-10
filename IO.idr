readNat : IO (Maybe Nat)
readNat = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (integerToNat (cast input)))
    else pure Nothing

guessNumber : (target : Nat) -> (tryNum : Nat) -> IO ()
guessNumber target tryNum = do
  putStrLn $ "[" ++ (show tryNum) ++ "] Guess the number:"
  Just g <- readNat
    | Nothing => do
        putStrLn "Invalid guess, please try again."
        guessNumber target tryNum
  case compare g target of
    LT => do
      putStrLn "The guess is too low."
      guessNumber target (tryNum + 1)
    GT => do
      putStrLn "The guess is too high."
      guessNumber target (tryNum + 1)
    EQ =>
      putStrLn "Your guess is correct."


printLength : IO ()
printLength =
  putStr "Input string: " >>= \_ =>
  getLine >>= \input =>
  let len = length input in
  putStrLn (show len)
