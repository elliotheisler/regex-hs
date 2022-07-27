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
{-
                                                                                               slowFactCont 3
                                                                            slowFactCont 2 >>= ( \acc -> return $ acc * 3 )
                                       ( slowFactCont 1 >>= ( \acc -> return $ acc * 2 ) ) >>= ( \acc -> return $ acc * 3 )
    ( ( slowFactCont 0 >>= ( \acc -> return $ acc*1 ) ) >>= ( \acc -> return $ acc * 2 ) ) >>= ( \acc -> return $ acc * 3 )
    ( (       return 1 >>= ( \acc -> return $ acc*1 ) ) >>= ( \acc -> return $ acc * 2 ) ) >>= ( \acc -> return $ acc * 3 )
    ( (       return 1 >>= ( \acc -> return $ acc*1 ) ) >>= ( \acc -> return $ acc * 2 ) )     ( \acc -> return $ acc * 3 )
    ( (       return 1 >>= ( \acc -> return $ acc*1 ) )     ( \acc -> return $ acc * 2 ) )     ( \acc -> return $ acc * 3 )
    ( (       return 1     ( \acc -> return $ acc*1 ) )     ( \acc -> return $ acc * 2 ) )     ( \acc -> return $ acc * 3 )
    ( (       ( \acc -> return $ acc * 1 ) 1            )   ( \acc -> return $ acc * 2 ) )     ( \acc -> return $ acc * 3 )
    ( (       ()         return 1         )              )   ( \acc -> return $ acc * 2 ) )     ( \acc -> return $ acc * 3 )

    (         ( \acc -> return $ acc * 2 ) 1                                             )     ( \acc -> return $ acc * 3 )
    (         (         return 2         )                                               )     ( \acc -> return $ acc * 3 )

              ( \acc -> return $ acc * 3 ) 2
              (         return 6         )





-}

