main = do
    n <- readLn :: IO Int
    if n == 42 then return ()
    else do
        print n
        main
