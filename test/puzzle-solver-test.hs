import Test.HUnit

test1 = TestCase (assertEqual "2 + 2 should equal 4" (2 + 2) 4)

allTests = TestList [ TestLabel "addition test" test1]
 
runTests = do runTestTT allTests
