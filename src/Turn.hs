module Turn where

import Control.Monad(unless)
import AppTypes


doTurn :: Interact ()
doTurn =
  do doPreTurn
     end <- checkEndRound
     if end
        then pure ()
        else
          do -- playing cards start here so set phase
             rested <- checkRest
             unless rested regularTurn
             endTurn


-- village, tctics (mostly night?), skills
doPreTurn :: Interact ()
doPreTurn = pure ()

checkEndRound :: Interact Bool
checkEndRound = pure False

checkRest :: Interact Bool
checkRest = pure False

regularTurn :: Interact ()
regularTurn = pure ()

endTurn :: Interact ()
endTurn = pure ()
