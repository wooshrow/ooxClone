module Data.Stack where

newtype Stack a = Stack { unStack :: [a] }

instance Functor Stack where
    fmap f Stack{unStack} = Stack $ map f unStack

singleton :: a -> Stack a
singleton x = Stack [x]

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x Stack{unStack} = Stack $ x : unStack

peek :: Stack a -> Maybe a
peek Stack{unStack}
    | not (Prelude.null unStack) = Just $ head unStack
    | otherwise                  = Nothing

peekUnsafe :: Stack a -> a
peekUnsafe stack
    | Just value <- peek stack = value
    | otherwise                = error "peekUnsafe: empty stack"

pop :: Stack a -> Stack a
pop Stack{unStack} = Stack $ tail unStack

size :: Stack a -> Int
size Stack{unStack} = length unStack

null :: Stack a -> Bool
null stack = size stack > 0 

updateTop :: a -> Stack a -> Stack a
updateTop new Stack{unStack}
    | (_:xs) <- unStack = Stack (new:xs)
    | otherwise         = error "cannot update an empty stack."