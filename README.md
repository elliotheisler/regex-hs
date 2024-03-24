# regex-hs
An implementation a subset of regular expression parsing and execution. I worked on this repository after my 3rd year of university to learn some of the Haskell ecosystem and apply accepted software practices, such as:
- Test-Driven Development
- making incremental, small commits with Git
- working with existing libraries. Notable are the use of the `parsec` library for parsing, and `HSpec` + `QuickCheck` for unit testing
## Running tests
`stack test`
## Example Usages
run `stack ghci` for interactive testing of implemented functionality
### Parse a regex
```
ghci> let emails = mkRegex "[a-zA-Z]\\w*@(gmail|outlook)\\.(com|ca|org)"
ghci> emails

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
### Apply a regex
The functions `reMatch`, `reMatches`, `reMatchSearch`, and `reMatchesSearch` family of
functions apply regular expressions, with `reMatchesSearch` doing the most work. 
- reMatch stops the search at the first match,
while `reSearches` continues the backtracking search until all are found. 
- The
`reMatch(es)Search` functions consider matches starting from all positions in the
string, as opposed to just the start.

```
ghci> reMatch emails "phyllis@gmail.com, greg@outlook.ca"
Just
Consumed : "phyllis@gmail.com"
Remaining: ", greg@outlook.ca"
Groups   : [CaptureGroup "gmail",CaptureGroup "com"]

ghci> reMatchesSearch emails "phyllis@gmail.com, greg@outlook.ca"
[
Consumed : "phyllis@gmail.com"
Remaining: ", greg@outlook.ca"
Groups   : [CaptureGroup "gmail",CaptureGroup "com"]
,
Consumed : "hyllis@gmail.com"
Remaining: ", greg@outlook.ca"
Groups   : [CaptureGroup "gmail",CaptureGroup "com"]
,
Consumed : "yllis@gmail.com"
Remaining: ", greg@outlook.ca"
Groups   : [CaptureGroup "gmail",CaptureGroup "com"]
,
Consumed : "llis@gmail.com"
Remaining: ", greg@outlook.ca"
Groups   : [CaptureGroup "gmail",CaptureGroup "com"]
,
Consumed : "lis@gmail.com"
Remaining: ", greg@outlook.ca"
Groups   : [CaptureGroup "gmail",CaptureGroup "com"]
,
Consumed : "is@gmail.com"
Remaining: ", greg@outlook.ca"
Groups   : [CaptureGroup "gmail",CaptureGroup "com"]
,
Consumed : "s@gmail.com"
Remaining: ", greg@outlook.ca"
Groups   : [CaptureGroup "gmail",CaptureGroup "com"]
,
Consumed : "greg@outlook.ca"
Remaining: ""
Groups   : [CaptureGroup "outlook",CaptureGroup "ca"]
,
Consumed : "reg@outlook.ca"
Remaining: ""
Groups   : [CaptureGroup "outlook",CaptureGroup "ca"]
,
Consumed : "eg@outlook.ca"
Remaining: ""
Groups   : [CaptureGroup "outlook",CaptureGroup "ca"]
,
Consumed : "g@outlook.ca"
Remaining: ""
Groups   : [CaptureGroup "outlook",CaptureGroup "ca"]
]
```
### Implemented Parts
- The basics: unions, concatenation, and quantifiers
- Greedy and non-greediness for quantifiers
- Character classes
- Parenthesis to enforce precedence
- Some character class literals such as `\w` for word characters
### Bugs and Features Not Yet Implemented
see `BUGS.txt` in `src` directory
