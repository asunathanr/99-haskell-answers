import Test.QuickCheck
import OneTen

prop_test1BaseCase :: [Int] -> Bool
prop_test1BaseCase [] = [0] == [0]
prop_test1BaseCase li = (myLast li) == (head (reverse li))