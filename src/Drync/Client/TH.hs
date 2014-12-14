module Drync.Client.TH (credentials) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

credentials :: QuasiQuoter
credentials = quoteFile credentialsQ

credentialsQ :: QuasiQuoter
credentialsQ = QuasiQuoter
    { quoteExp = credentialsExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

credentialsExp :: String -> Q Exp
credentialsExp str =
    return $ case map words $ lines str of
        (["CLIENT_ID", cid]:["CLIENT_SECRET", cs]:_) -> tup cid cs
        _ -> tup "" ""

  where
    tup x y = TupE [LitE (StringL x), LitE (StringL y)]
