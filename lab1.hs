-- Сегментне дерево для обчислення асоціативної операції
data SegmentTree a = SegmentTree Int (a -> a -> a) a [a]

-- Побудова сегментного дерева
buildSegmentTree :: (a -> a -> a) -> a -> [a] -> SegmentTree a
buildSegmentTree op idVal arr = SegmentTree n op idVal (build 0 (n - 1))
  where
    n = length arr
    build l r
      | l == r = [arr !! l]
      | otherwise =
          let mid = (l + r) `div` 2
              leftTree = build l mid
              rightTree = build (mid + 1) r
          in [op (head leftTree) (head rightTree)] ++ leftTree ++ rightTree

-- Обчислення операції на діапазоні [l, r)
queryRange :: SegmentTree a -> Int -> Int -> a
queryRange (SegmentTree n op idVal tree) l r = query 0 (n - 1) 0 l r
  where
    query lBound rBound pos l r
      | l > rBound || r < lBound = idVal  -- Діапазон поза межами
      | l <= lBound && rBound <= r = tree !! pos  -- Повне перекриття
      | otherwise =
          let mid = (lBound + rBound) `div` 2
              leftResult = query lBound mid (2 * pos + 1) l r
              rightResult = query (mid + 1) rBound (2 * pos + 2) l r
          in op leftResult rightResult

-- Основна функція для обчислення результатів для всіх діапазонів
rangeResults :: (a -> a -> a) -> a -> [a] -> [(Int, Int)] -> [a]
rangeResults op idVal arr ranges = map (\(l, r) -> queryRange segTree l (r - 1)) ranges
  where
    segTree = buildSegmentTree op idVal arr

-- Приклад використання
main :: IO ()
main = do
    let arr = [3, 7, 9, 5, 2]         -- Масив елементів
    let ranges = [(2, 3), (0, 6)]        -- Масив діапазонів
    let op = (+)                         -- Операція
    let idVal = 0                        -- Нейтральний елемент для додавання
    let results = rangeResults op idVal arr ranges  -- Обчислення результатів
    print results                         -- Виведення результатів
