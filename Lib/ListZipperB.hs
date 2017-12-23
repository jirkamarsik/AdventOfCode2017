module ListZipperB where

data ListZipper a = LZ [a] [a]

toList :: ListZipper a -> [a]
toList (LZ ls rs) = reverse ls ++ rs

move :: Integer -> ListZipper a -> Maybe (ListZipper a)
move 0 lz                     = Just lz
move d (LZ ls (r:rs)) | d > 0 = move (d - 1) (LZ (r:ls) rs)
move d (LZ (l:ls) rs) | d < 0 = move (d + 1) (LZ ls (l:rs))
move _ _                      = Nothing

advanceC :: Int -> ListZipper a -> ListZipper a
advanceC 0 lz             = lz
advanceC d (LZ ls [])     = advanceC d       (LZ []     (reverse ls))
advanceC d (LZ ls (r:rs)) = advanceC (d - 1) (LZ (r:ls) rs)

headC :: ListZipper a -> a
headC (LZ _  (r:_)) = r
headC (LZ ls [])    = last ls

insert :: a -> ListZipper a -> ListZipper a
insert x (LZ ls rs) = LZ ls (x:rs)

locateC :: Eq a => a -> ListZipper a -> ListZipper a
locateC x lz = if headC lz == x then
                 lz
               else
                 locateC x $ advanceC 1 lz

flipNextC :: Int -> ListZipper a -> ListZipper a
flipNextC n (LZ ls rs) = LZ ls' rs'
  where subseq = take n_r rs ++ take n_l (reverse ls)
        revSubseq = reverse subseq
        rs' = take n_r revSubseq ++ drop n_r rs
        ls' = reverse (drop n_r revSubseq ++ drop n_l (reverse ls))
        n_r = min n (length rs)
        n_l = n - n_r
