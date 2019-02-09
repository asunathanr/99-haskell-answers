import Test.HUnit

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1,2))

tests = TestList [TestLabel "test1" test1]