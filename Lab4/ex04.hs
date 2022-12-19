{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


--data MyInt = MkMyInt Int
newtype MyInt = MkMyInt Int

instance Eq MyInt where
  (==) :: MyInt -> MyInt -> Bool
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) :: MyInt -> MyInt -> Bool
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) :: MyInt -> MyInt -> MyInt
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)

  (-) :: MyInt -> MyInt -> MyInt
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)

  (*) :: MyInt -> MyInt -> MyInt
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)

  negate :: MyInt -> MyInt
  negate (MkMyInt i)            = MkMyInt (negate i)

  abs :: MyInt -> MyInt
  abs (MkMyInt i)               = MkMyInt (abs i)

  signum :: MyInt -> MyInt
  signum (MkMyInt i)            = MkMyInt (signum i)

  fromInteger :: Integer -> MyInt
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show :: MyInt -> String
  show (MkMyInt i) = "MkMyInt " ++ show i

