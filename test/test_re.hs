import Test.QuickCheck (quickCheck)
prop_char char = parse parsePrimary "" [char] == Symbol char
