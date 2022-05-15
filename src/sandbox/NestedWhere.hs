module NestedWhere where

append_3 a b =
    a `plusplus` b
    where 
    this = "asf"
    plusplus a b = 
        a + b
        where
        infixr 5 +
        (+) a b = a ++ b

append_2 a b =
    a `plusplus` b
  where 
    plusplus a b = 
        a + b
      where
        infixr 5 +
        (+) a b = a ++ b

append_1 a b =  a `plusplus` b
  where 
    plusplus a b = a + b
      where
        infixr 5 +
        (+) a b = a ++ b

append_0 a b = a `plusplus` b
    where 
        plusplus a b = a + b
            where
                infixr 5 +
                (+) a b = a ++ b