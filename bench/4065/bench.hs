import Criterion.Main

foo :: Int -> Int -> Int
foo n k | n <= 0    = k
        | otherwise = foo (n-1) (k+1)

bar :: Int -> Int -> Int
bar n k | n == 0    = k
        | otherwise = bar (n-1) (k+1)

main :: IO ()
main = defaultMain [ bench "foo" $ nf (uncurry foo) (20000000,0)
                   , bench "bar" $ nf (uncurry bar) (20000000,0)
                   ]

