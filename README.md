# regex-hs
A regular expression engine in haskell, for fun. Still a work in progress.
## Cool Features
### Print Things in a Tree Format
A typeclass that allows any self-referential data ADT to be printed as a tree, a-la the Unix "tree" command. Define the contents `ptContents` and children `ptForest` for each node type in the ADT, the the function `ptShow` can be called. Here is an example with a regular expression:

```
:{
let emails = fromRight undefined . reCompile $ 
        "[a-zA-Z]\\w*@(gmail|outlook)\\.(com|ca|org)" :: RETree

in  putStrLn $ ptShow emails
:}
        ┌[a-z] 
    ┌[|]┴[A-Z] 
    │       ┌[a-z] 
    │       ├[A-Z] 
    ├(*)─[|]┼[0-9] 
    │       └[_] 
    ├@ 
    │           ┌g 
    │           ├m 
    │      ┌(<>)┼a 
    │      │    ├i 
    │      │    └l 
    │      │    ┌o 
    │      │    ├u 
    │      │    ├t 
(<>)┼()─(|)┴(<>)┼l 
    │           ├o 
    │           ├o 
    │           └k 
    ├. 
    │           ┌c 
    │      ┌(<>)┼o 
    │      │    └m 
    │      │    ┌c 
    └()─(|)┼(<>)┴a 
           │    ┌o 
           └(<>)┼r 
                └g 
```

### Backtrack Every Possible Match:
`reMatches :: (RegexRepr r) => r -> String -> [MatchProgress]`

With Haskell's list monad, it is easy to yield every possible match from a regex. Also, due to Haskell's lazy evaluation, you can simply take the head of the result of `reMatches` if you only need one match, without evaluating every result:
```
reMatch :: (RegexRepr r) => r -> String -> Maybe MatchProgress
reMatch r input = case reMatches r input of
    h:tail -> Just h
    [] -> Nothing
```