{-# LANGUAGE InstanceSigs #-}

module FantasyLand where 

{- Definí el tipo Parser, cuyo kind es * -> *: 
Un valor de tipo Parser a es una función que
toma una String (que representa el resto de la entrada 
en un momento dado) y devuelve o Nothing si el parsing 
fracasa, o sino devuelve un valor de tipo a y el resto
de la entrada.-}

data Parser a = P {
    {-Usá esto en lugar de hacer pattern matching explícito 
      en las funciones más abajo, porque sino ciertas definiciones 
      recursivas van a entrar en un bucle, porque el pattern matching 
      (y así la evaluación) ocurre demasiado temprano.-}
    runParser :: String -> Maybe (a, String)
}

{-
 Es el punto de entrada principal para ejecutar un parser. 
 Debe devolver un valor exitoso solo si el parser consumió toda la entrada, 
 es decir, si la función dentro de Parser a devuelve un valor de tipo a
 junto con una String vacía.
-}
parse :: Parser a -> String -> Maybe a
parse p str = do 
    (a, s) <- runParser p $ str
    if(s == "") then Just a
    else Nothing
   
{-
 Representa un parser que falla siempre.
 Deberías tener
 parse noParser input == Nothing
 para cualquier entrada input, incluso la String vacía.
-}
noParser :: Parser a
noParser = P { runParser = (\_ -> Nothing)}

{-
Representa el parser que no consume ninguna entrada 
y devuelve su argumento.
Deberías tener
parse (pureParser x) "" == Just x
xs ≠ "" ⇒ parse (pureParser x) xs == Nothing
-}
pureParser :: a -> Parser a
pureParser a = P { runParser = (\input -> Just(a, input) )}

{-
Deberías tener
parse (fmap f p) input == fmap f (parse p input)
para cualquier f, p e input.
parse (fmap (\x -> x + 2) pureParser 4) "" == fmap (\x -> x + 2) (parse (pureParser 4) "" )
-}
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P { runParser = functorRunner }
        where functorRunner = (\input ->  
                case runParser p $ input of
                    Nothing -> Nothing
                    Just (a, s) -> Just (f a, s))
        
{-
aplicá el parser de la izquierda a la entrada primera para conseguir la función. 
Si tiene éxito, aplicá el parser de la derecha al resto de la entrada para conseguir el argumento, 
y devuelve la función aplicada al argumento, y el resto de la entrada en el argumento correcto.
-}
instance Applicative Parser where
    pure = pureParser
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    fp <*> fx = P { runParser = applicativeRunner }
        where applicativeRunner = (\input ->  
                case runParser fp $ input of
                    Nothing -> Nothing
                    Just (f, s) -> runParser (fmap f fx) $ s)
            

instance Monad Parser where
    return = pureParser
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    fa >>= k = P { runParser = monadRunner }
        where monadRunner = (\input ->  
                case runParser fa $ input of
                    Nothing -> Nothing
                    Just (a, s) -> runParser (k a) $ s)
        

-- Tests
empty_input :: String
empty_input = ""
no_empty_input :: String
no_empty_input = "no_empty"
x :: Int 
x = 4
test_no_parser_empty = parse noParser empty_input == (Nothing :: Maybe Int)
test_no_parser_no_empty = parse noParser no_empty_input == (Nothing :: Maybe Int)
test_pure_empty = parse (pureParser x) empty_input == Just x
test_pure_no_empty = parse (pureParser x) no_empty_input == Nothing
test_parser :: Parser Int
test_parser = pureParser x
f_test :: Int -> String
f_test = show

-- Test Function
test_parsers p1 p2 input = (runParser p1 $ input) == (runParser p2 $ input)

-- Test Functor Laws => Not yet finished
-- Identity :: fmap id = id
test_functor_identity = 
    let m = pureParser 4
        parserA = id m
        parserB = fmap id m
    in test_parsers parserA parserB ""
-- Associativity :: fmap (g . h) = (fmap g) . (fmap h)
test_functor_associativity = 
    let m = pureParser 4
        g = (*2)
        h = (+1)
        parserA = fmap (g . h) m
        parserB = (fmap g) . (fmap h) $ m
    in test_parsers parserA parserB ""

test_funtor_empty = parse (fmap f_test test_parser) empty_input == fmap f_test (parse test_parser empty_input)
test_funtor_no_empty = parse (fmap f_test test_parser) no_empty_input == fmap f_test (parse test_parser no_empty_input)

-- Test Applicative Laws => Not yet finished
test_applicative_empty = parse (pureParser f_test <*> test_parser) empty_input == Just "4"
test_applicative_no_empty = parse (pureParser f_test <*> test_parser) no_empty_input == Nothing

-- Test Monad Laws
-- Left identity :: return a >>= f ≡ f a
test_monad_left_identity = 
    let a = 4
        f = pureParser . (+1)
        parserA = pureParser a >>= f
        parserB = f a
    in test_parsers parserA parserB ""
-- Right identity :: m >>= return ≡ m
test_monad_right_identity = 
    let m = pureParser 4
        parserA = m >>= return
        parserB = m
    in test_parsers parserA parserB  ""
-- Associativity :: (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
test_monad_associativity = 
    let m = pureParser 4
        f = pureParser . (+1)
        g = pureParser . (*2)
        parserA = (m >>= f) >>= g
        parserB = m >>= (\x -> f x >>= g)
    in test_parsers parserA parserB ""

tests = [test_no_parser_empty,
    test_no_parser_no_empty,
    test_pure_empty,
    test_pure_no_empty,
    test_functor_identity,
    test_functor_associativity,
    test_funtor_empty,
    test_funtor_no_empty,
    test_applicative_empty,
    test_applicative_no_empty,
    test_monad_left_identity,
    test_monad_right_identity,
    test_monad_associativity]