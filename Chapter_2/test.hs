sayHello :: String -> IO()
sayHello x =
    putStrLn("Hello, " ++ x ++ "!")

triple x = x*3

half x = x/2

square x = x*x

circleArea r = pi*(r*r)

perimeter x y = x*2 + y*2

foo x =
    let y = x*2
        z = x^2
    in 2*y*z

area x = 3.14 * (x*x)

double x = x*2

x=7
y=10
f = x+y