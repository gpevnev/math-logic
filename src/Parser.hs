module Parser
where
import           Control.Applicative     hiding ( many )
import           Data.Char                      ( isDigit
                                                , isLower
                                                , isSpace
                                                , isUpper
                                                )

newtype Prs a = Prs { apply:: String -> Maybe (a, String) }

instance Functor Prs where
    f `fmap` p = Prs fun where
        fun s = g <$> apply p s
        g (a, s) = (f a, s)

instance Applicative Prs where
    pure x = Prs f where
        f s = Just (x, s)

    pf <*> pa = Prs fun where
        fun s = case apply pf s of
            Nothing      -> Nothing
            Just (f, s') -> g f <$> apply pa s'
        g f (a, s) = (f a, s)

instance Monad Prs where
    ma >>= k = Prs fun where
        fun s = case apply ma s of
            Nothing      -> Nothing
            Just (a, s') -> apply (k a) s'

instance Alternative Prs where
    empty = Prs $ const Nothing

    ma1 <|> ma2 = Prs fun where
        fun s = apply ma1 s <|> apply ma2 s

satisfy :: (Char -> Bool) -> Prs Char
satisfy p = Prs f
  where
    f (c : cs) | p c       = Just (c, cs)
               | otherwise = Nothing
    f [] = Nothing

char :: Char -> Prs Char
char c = satisfy (== c)

anyChar :: Prs Char
anyChar = satisfy $ const True

string :: String -> Prs String
string (c : cs) = (:) <$> char c <*> string cs
string []       = pure []

newLine :: Prs Char
newLine = char '\n'

digit :: Prs Char
digit = satisfy isDigit

lower :: Prs Char
lower = satisfy isLower

upper :: Prs Char
upper = satisfy isUpper

space :: Prs Char
space = satisfy isSpace

many :: Prs a -> Prs [a]
many prs = (:) <$> prs <*> many prs <|> pure []

many1 :: Prs a -> Prs [a]
many1 prs = (:) <$> prs <*> (many prs <|> pure [])

sepBy :: Prs a -> Prs b -> Prs [a]
pm `sepBy` ps = (:) <$> pm <*> many (ps *> pm) <|> pure []

endBy :: Prs a -> Prs b -> Prs [a]
pm `endBy` ps = many (pm <* ps)

chainr :: Prs a -> Prs (a -> a -> a) -> Prs a
chainr mem op = scan
  where
    scan = do
        x <- mem
        rest x

    rest x =
        (do
                f <- op
                y <- scan
                return $ f x y
            )
            <|> return x

chainl :: Prs a -> Prs (a -> a -> a) -> Prs a
chainl mem op = do
    x <- mem
    rest x
  where
    rest x =
        (do
                f <- op
                y <- mem
                rest $ f x y
            )
            <|> return x

parse :: Prs a -> String -> a
parse p s = case apply p s of
    Just (a, s') -> if null s' then a else a -- error $ "got extra \"" ++ s' ++ "\""
    Nothing      -> error "cannot parse"

