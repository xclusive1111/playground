number1 :: Num a => a
number1 = 1 + 5 + 7 + 3 + 2

number2 :: Num a => a
number2 = number1 * number1

main :: IO ()
main = print number2
