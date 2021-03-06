module ListZipperA where

data ListZipper a = LZ [a] a [a]

toList :: ListZipper a -> [a]
toList (LZ ls h rs) = (reverse ls) ++ h:rs

move :: Int -> ListZipper a -> Maybe (ListZipper a)
move 0 zipper                   = Just zipper
move d (LZ (l:ls) h rs) | d < 0 = move (d + 1) (LZ ls l (h:rs))
move d (LZ ls h (r:rs)) | d > 0 = move (d - 1) (LZ (h:ls) r rs)
move _ _                        = Nothing

advanceC :: Int -> ListZipper a -> ListZipper a
advanceC 0 lz               = lz
advanceC d (LZ ls h (r:rs)) = advanceC (d - 1) (LZ (h:ls) r rs)
advanceC d lz               = advanceC (d - 1) (resetCursor lz)

getCursor :: ListZipper a -> a
getCursor (LZ _ h _) = h

updateCursor :: (a -> a) -> ListZipper a -> ListZipper a
updateCursor f (LZ ls h rs) = LZ ls (f h) rs

resetCursor :: ListZipper a -> ListZipper a
resetCursor lz = LZ [] x xs
  where x:xs = toList lz

locate :: Eq a => a -> [a] -> Maybe (ListZipper a)
locate _ []     = Nothing
locate n (x:xs) = search n (LZ [] x xs)
  where search :: Eq a => a -> ListZipper a -> Maybe (ListZipper a)
        search n (LZ ls h rs) | h == n = Just $ LZ ls h rs
        search n (LZ ls h (r:rs))      = search n (LZ (h:ls) r rs)
        search _ (LZ _ _ [])           = Nothing
