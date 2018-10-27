##### [https://cs.famaf.unc.edu.ar/~hoffmann/pd18/practico02.html#una-m%C3%B3nada-para-construir-parsers-parseando-archivos-csv](https://cs.famaf.unc.edu.ar/~hoffmann/pd18/practico02.html#una-m%C3%B3nada-para-construir-parsers-parseando-archivos-csv)

## A monad for parser construction: 
# Parsing CSV files

A good example of how practival monads are, is the one about parser combinators.
In this exercise, we will construct a little library of parser combinators.
```haskell
parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content
```
