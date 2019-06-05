x = undefined
y = "blah"
main = do
    print $ x `seq` (snd (x `seq` x, y))