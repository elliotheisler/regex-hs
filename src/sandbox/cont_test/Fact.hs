import Control.Monad.Cont

fact :: Integer -> Integer -> Integer
fact acc 0 = acc
fact acc n = fact (acc*n) (n-1)

factCont :: Integer -> Integer -> Cont r Integer
factCont acc 0 = return acc
factCont acc n = return (n-1) >>= factCont (acc*n)


slowFact :: Integer -> Integer
slowFact 0 = 1
slowFact n = n * slowFact (n-1)

slowFactCont :: Integer -> Cont r Integer
slowFactCont 0 = return 1
slowFactCont n = slowFactCont (n-1) >>= ( \acc -> return $ acc*n )

