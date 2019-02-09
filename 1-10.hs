-- Answers to problems 1 to 10, questions found here: https://wiki.haskell.org/99_questions/

-- Problem 1
myLast [x] = x
myLast li = myLast $ tail $ li

