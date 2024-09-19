{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Створимо синоніми типів для Int і Maybe
type IntegerAlias = Int
type Option a = Maybe a

-- Створимо новий клас типів на основі Eq
class Equal a where
    isEqual :: a -> a -> Bool

-- Інстанція для типу IntegerAlias (який є синонімом Int)
instance Equal IntegerAlias where
    isEqual x y = x == y

-- Інстанція для типу Maybe
instance (Equal a) => Equal (Option a) where
    isEqual Nothing Nothing = True
    isEqual (Just x) (Just y) = isEqual x y
    isEqual _ _ = False

-- Створимо новий клас типів на основі Show
class Display a where
    display :: a -> String

-- Інстанція для типу IntegerAlias
instance Display IntegerAlias where
    display x = show x

-- Інстанція для типу Maybe
instance (Display a) => Display (Option a) where
    display Nothing = "Nothing"
    display (Just x) = "Just " ++ display x

-- Приклад використання нових типів і класів
main :: IO ()
main = do
    let x = 5 :: IntegerAlias
    let y = Just 10 :: Option IntegerAlias
    print (isEqual x x)            -- Використовуємо Equal для порівняння
    putStrLn (display y)           -- Використовуємо Display для виведення
