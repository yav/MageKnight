const mana = [ "Red", "Green", "White", "Blue", "Gold", "Black" ]

const constant = {}

function setupConstants() {
  constant.cardWidth       = 220
  constant.cardHeight      = constant.cardWidth * 7 / 5
  constant.smallCardWidth  = 0.7 * constant.cardWidth
  constant.smallCardHeight = 0.08 * constant.cardHeight

  constant.dieSize         = 32
  constant.buttonHeight    = 32
  constant.buttonWidth     = 64

  const glob = document.documentElement.style
  for (c in constant) {
    glob.setProperty("--" + c, constant[c])
    glob.setProperty("--" + c + "_px", constant[c] + "px")
  }
}
