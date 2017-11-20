module Mossy.QQ where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

--------------------------------------------------------------------------------
-- QQ
--------------------------------------------------------------------------------
normaliseNewlines :: String -> String
normaliseNewlines []             = []
normaliseNewlines ('\r':'\n':cs) = '\n':normaliseNewlines cs
normaliseNewlines (c:cs)         = c:normaliseNewlines cs

-- | A raw string quasiquoter. Extracted from dead-simple-json.
str :: QuasiQuoter
str =
  QuasiQuoter { quoteExp  = return . LitE . StringL . normaliseNewlines
              , quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                                       \(allowed as expression only, used as a pattern)"
              , quoteType = \_ -> fail "illegal raw string QuasiQuote \
                                       \(allowed as expression only, used as a type)"
              , quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                                       \(allowed as expression only, used as a declaration)"
              }
