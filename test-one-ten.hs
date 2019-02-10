import Test.QuickCheck
import OneTen

prop_test1BaseCase :: [Int] -> Bool
prop_test1BaseCase [] = [0] == [0]
prop_test1BaseCase li = (myLast li) == (head (reverse li))

prop_testMyButLast :: [Int] -> Bool
prop_testMyButLast [] = [0] == [0]
prop_testMyButLast li = (myButLast li) == (head (tail (reverse li)))



prop_testMyReverse :: [Int] -> Bool
prop_testMyReverse li = (reverse li) == (myReverse li)