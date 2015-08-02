module MageKnight.Game1 where

import MageKnight.Player
import MageKnight.Land
import MageKnight.Offers
import MageKnight.Source

{-
1. Fame & Reputation: in Player
2. Enemy & Ruin tokens: in Land
3. Artifact deck: in Offers
4. Wound pile: N/A
5. Spell deck & spell offer: in Offers
6. Advanced action deck and advanced offer: in Offers
7. Common skill offer: in Offer
8. Regular and Elite units: in Offers
9. Unit offer: in Offers
10. Round order tokens: XXX
11. Day/Night board
  - Time & move costs: in Land
  - Source: here
12. Active tactics: XXX
13. Inactive tactics: XXX
14. Tile deck: in Land
15. Map: in Land
16. Site description cards & scoring card: N/A
17. City cards: in Land
18. Bank: N/A
-}

data Game = Game
  { gamePlayers :: [Player]
  , gameLand    :: Land
  , gameOffers  :: Offers
  , gameSource  :: Source
  }


