
export 
type Mana = (typeof mana)[number]

export
type HexLoc = (typeof hex_locs)[number]


export
const mana = [ "Red", "Green", "White", "Blue", "Gold", "Black" ]

export
const hex_locs = [ "Center", "NE", "E", "SE", "SW", "W", "NW" ]

export
let constant :
  { cardWidth: number,
    cardHeight: number,
    smallCardWidth: number,
    smallCardHeight: number,

    buttonHeight: number,
    buttonWidth: number,

    dieSize: number,

    tileSize: number,
    hexWidth: number,
    hexHeight: number,
    charWidth: number,
    charHeight: number,

    enemySize: number,
    ruinsWidth: number,
    ruinsHeight: number
  }

export
function setupConstants() {
  constant.cardWidth       = 220
  constant.cardHeight      = constant.cardWidth * 7 / 5
  constant.smallCardWidth  = 0.7 * constant.cardWidth
  constant.smallCardHeight = 0.08 * constant.cardHeight

  constant.dieSize         = 48

  constant.buttonHeight    = 32
  constant.buttonWidth     = 64

  const mapScale = 2

  constant.tileSize        = mapScale * 300
  constant.hexWidth        = constant.tileSize / 3
  constant.hexHeight       = constant.tileSize  * 0.4
  constant.charWidth       = constant.hexWidth  * 0.4
  constant.charHeight      = constant.hexHeight * 0.4

  constant.enemySize       = constant.hexWidth * 0.4
  constant.ruinsWidth      = constant.hexWidth * 0.35
  constant.ruinsHeight     = constant.hexWidth * 0.4

  const glob = document.documentElement.style
  for (const c in constant) {
    glob.setProperty("--" + c, constant[c])
    glob.setProperty("--" + c + "_px", constant[c] + "px")
  }
}
