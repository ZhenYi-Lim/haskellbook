module Hello 
    ( sayHello )
    where
--specifies which top level bindings are to be exported.
--without () all will be exported.

sayHello :: String -> IO()
sayHello name = do
    putStrLn ("Hi " ++ name ++ "!")