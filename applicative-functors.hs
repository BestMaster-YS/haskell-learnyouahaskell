import Control.Applicative

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f a b = f <$> a <*> b

sequenceA' :: (Applicative f) => [f a] -> f [a]
-- sequenceA [] = pure []
-- sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
sequenceA' = foldr (liftA2 (:)) (pure [])

newtype CharList = CharList { getCharList :: String } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"