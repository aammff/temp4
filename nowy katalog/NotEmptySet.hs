data NotEmptySet a = Full | NotFull (NotEmptySet a) a deriving Show

full :: NotEmptySet a
full = Full

isFull :: NotEmptySet a -> Bool
isFull Full = True
isFull _ = False

delete :: Eq a => a -> NotEmptySet a -> NotEmptySet a
delete x Full = NotFull Full x
delete x as@(NotFull a y) = if y == x then as else NotFull (delete x a) y

insert :: Eq a => a -> NotEmptySet a -> NotEmptySet a
insert x Full = Full
insert x (NotFull a y) = if y == x then a else NotFull (insert x a) y

notMember :: Eq a => a -> NotEmptySet a -> Bool
notMember x Full = False
notMember x (NotFull a y) = if y == x then True else notMember x a

member :: Eq a => a -> NotEmptySet a -> Bool
member x a = not $ notMember x a
