module Y2020.M08.D19.Solution where

{--
SKRIBBL.io ... part uno

Sucribl.io is a social game online where you try to guess the word (first) from
the scribbles of the ... poser (?) ... is that the correct word? from the
scribbles on screen the poser draws. You have a hint in the number of letters


Like this:

_ _ _ _ _


for the word "alien"

And, over time, letters are revealed to help the game along, like this:


_ _ i _ _


ALSO, if you guess, say: "aliem" skribbl.io will say: "CLOSE!" ... so, if the
edit-distance is one, then you're super-close and skribbl.io tells you.

So, we'll break this problem of skribbl.io down over several days.

The first one is:

given a (set of?) dictionary, e.g. (on some computers, YMMV):

/usr/share/dict/propernames
/usr/share/dict/words

find the words of length x from those dictionaries.
--}

import Control.Monad (join)

import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

prefix :: String -> String
prefix = ("/usr/share/dict/" ++)

dicts :: [FilePath]
dicts = map prefix (words "propernames words")

wordsOf :: FilePath -> Int -> IO [String]
wordsOf dict n = readFile dict >>=
                 return . map (map toLower) . filter ((n ==) . length) . lines

{--
What are all the five-letter words of both dictionaries propernames and words?

What code would you write to return this set of 5-letter words? How would
you guarantee that each word is unique? For example, there're the sentences:

"Susie pat the bunny." and "Pat, the bunny, ate the lettuce."

We don't want to see "pat" twice in the list from the different dictionary
sources.

And, of course, "Susie pet the bunny" is a non sequitur, but "Susie's pet
is a bunny" is not nonsensical. #English101
--}

allWordsOfLength :: Int -> IO (Set String)
allWordsOfLength n = mapM (flip wordsOf n) dicts >>=
                     return . Set.fromList . join

{--
>>> allWordsOfLength 5
fromList ["aalii","aaron","abaca","aback","abaff","abaft",...]
>>> length (allWordsOfLength 5)
9972
--}
