import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- module DFA
-- ( State
-- , delta
-- , DFA
-- , deltaHat_DFA
-- ) where


type State = String
type Input = String

type DeltaDFA = Map (State, Char) State
data DFA = DFA { transitionsDFA     :: DeltaDFA,
                 initStateDFA       :: State,
                 acceptingStatesDFA :: [State] }

type DeltaNFA = Map (State, Char) [State]
data NFA = NFA { transitionsNFA     :: DeltaNFA,
                 initStateNFA       :: State,
                 acceptingStatesNFA :: [State] }



deltaHat_DFA :: State -> Input -> DeltaDFA -> State
deltaHat_DFA q_init word transitions = foldl (\q w -> transitions ! (q, w)) q_init word

deltaHat_NFA :: [State] -> Input -> DeltaNFA -> [State]
deltaHat_NFA q_inits word transitions = 
  foldl (\qs w -> qs >>= (\q -> transitions ! (q, w))) q_inits word

deltaHat_NFA_Mb :: [State] -> Input -> DeltaNFA -> [State]
deltaHat_NFA_Mb init_states word transitions = foldl 
  (\ qs w -> qs >>= (\ q -> Maybe.fromMaybe [] $ Map.lookup (q, w) transitions)) 
  init_states 
  word


accepting :: DFA -> [Char] -> Bool
accepting (DFA _ _ []) _ = False
accepting dfa str = 
  let ending_state = deltaHat_DFA (initStateDFA dfa) str (transitionsDFA dfa)
  in ending_state `elem` (acceptingStatesDFA dfa) 


-- w contains "aa" as substring
trans = Map.fromList [
              (("q0", 'b'), "q0"),
              (("q0", 'a'), "q1"),
              (("q1", 'b'), "q0"),
              (("q1", 'a'), "q2"),
              (("q2", 'a'), "q2"),
              (("q2", 'b'), "q2") ]

transND = Map.fromList [
              (("q0", 'b'), ["q0"]),
              (("q0", 'a'), ["q1"]),
              (("q1", 'b'), ["q0"]),
              (("q1", 'a'), ["q2"]),
              (("q2", 'a'), ["q2"]),
              (("q2", 'b'), ["q2"]) ]

aSepB = Map.fromList [
              (("q0", 'a'), ["q0"]),
              (("q0", 'b'), []),
              (("q0", '#'), ["q1"]),
              (("q1", 'a'), []),
              (("q1", 'b'), ["q1"]),
              (("q1", '#'), ["q0"]) ]

asThenBs = Map.fromList [
              (("q0", 'a'), ["q0", "q1"]),
              (("q0", 'b'), []),
              (("q1", 'a'), []),
              (("q1", 'b'), ["q1"]) ]

asThenBs_Mb = Map.fromList [
              (("q0", 'a'), ["q0", "q1"]),
              (("q1", 'b'), ["q1"]      ) ]

main = do
       putStrLn $ deltaHat_DFA "q0" "" trans -- q0
       putStrLn $ deltaHat_DFA "q0" "a" trans
       putStrLn $ deltaHat_DFA "q0" "ab" trans -- q0
       putStrLn $ deltaHat_DFA "q0" "aa" trans
       putStrLn $ deltaHat_DFA "q0" "aabbabbababab" trans -- q2
       putStrLn $ deltaHat_DFA "q0" "abbbbabbbabbbbaba" trans -- q1
       putStrLn "===="
       putStrLn $ show $ deltaHat_NFA ["q0"] "" transND -- q0
       putStrLn $ show $ deltaHat_NFA ["q0"] "a" transND
       putStrLn $ show $ deltaHat_NFA ["q0"] "ab" transND -- q0
       putStrLn $ show $ deltaHat_NFA ["q0"] "aa" transND
       putStrLn $ show $ deltaHat_NFA ["q0"] "aabbabbababab" transND -- q2
       putStrLn $ show $ deltaHat_NFA ["q0"] "abbbbabbbabbbbaba" transND -- q1
       putStrLn "===="
       putStrLn $ show $ deltaHat_NFA ["q0"] "a" aSepB -- q0
       putStrLn $ show $ deltaHat_NFA ["q0"] "b" aSepB -- []
       putStrLn $ show $ deltaHat_NFA ["q0"] "#" aSepB -- q1
       putStrLn $ show $ deltaHat_NFA ["q0"] "aaa#bbb" aSepB -- q1
       putStrLn $ show $ deltaHat_NFA ["q0"] "aa##aaa" aSepB -- q0
       putStrLn $ show $ deltaHat_NFA ["q0"] "#bbbb" aSepB -- q1
       putStrLn $ show $ deltaHat_NFA ["q0"] "a#bbab" aSepB -- []
       putStrLn "===="
       putStrLn $ show $ deltaHat_NFA ["q0"] "a" asThenBs -- q0, q1
       putStrLn $ show $ deltaHat_NFA ["q0"] "b" asThenBs -- []
       putStrLn $ show $ deltaHat_NFA ["q0"] "ab" asThenBs -- q1
       putStrLn $ show $ deltaHat_NFA ["q0"] "aaaaaaabbbba" asThenBs -- []
       putStrLn $ show $ deltaHat_NFA ["q0"] "aaaaaaabbbbb" asThenBs -- q1
       putStrLn $ show $ deltaHat_NFA ["q0"] "aaaaaaa" asThenBs -- q0, q1
       putStrLn "===="
       putStrLn $ show $ deltaHat_NFA_Mb ["q0"] "a" asThenBs_Mb -- q0, q1
       putStrLn $ show $ deltaHat_NFA_Mb ["q0"] "b" asThenBs_Mb -- []
       putStrLn $ show $ deltaHat_NFA_Mb ["q0"] "ab" asThenBs_Mb -- q1
       putStrLn $ show $ deltaHat_NFA_Mb ["q0"] "aaaaaaabbbba" asThenBs_Mb -- []
       putStrLn $ show $ deltaHat_NFA_Mb ["q0"] "aaaaaaabbbbb" asThenBs_Mb -- q1
       putStrLn $ show $ deltaHat_NFA_Mb ["q0"] "aaaaaaa" asThenBs_Mb -- q0, q1
       
