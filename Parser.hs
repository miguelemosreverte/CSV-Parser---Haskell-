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
pureParser a = P{ runParser = (\s -> Just(a, s) )}

{-
Deberías tener
parse (fmap f p) input == fmap f (parse p input)
para cualquier f, p e input.
parse (fmap (\x -> x + 2) pureParser 4) "" == fmap (\x -> x + 2) (parse (pureParser 4) "" )
-}
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P{ runParser = (\input -> case runParser p $ input of
            Nothing -> Nothing
            Just (a, s) -> Just (f a, s)
        )}

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
test_funtor_empty = parse (fmap f_test test_parser) empty_input == fmap f_test (parse test_parser empty_input)
test_funtor_no_empty = parse (fmap f_test test_parser) no_empty_input == fmap f_test (parse test_parser no_empty_input)
tests = [test_no_parser_empty,
    test_no_parser_no_empty,
    test_pure_empty,
    test_pure_no_empty,
    test_funtor_empty,
    test_funtor_no_empty]