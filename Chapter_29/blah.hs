import Debug.Trace

blah :: IO String
blah = return "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

main :: IO ()
main = do
    b <- blah'  --"outer trace"
    putStrLn b  --"blah"
    putStrLn b  --"blah"
    w <- woot   --"inner trace"
    putStrLn w  --"woot"
    putStrLn w  --"woot"