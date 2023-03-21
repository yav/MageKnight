const constant = {}

function setupConstants() {
  constant.cardWidth       = 220
  constant.cardHeight      = constant.cardWidth * 7 / 5
  constant.smallCardWidth  = 0.7 * constant.cardWidth
  constant.smallCardHeight = 0.08 * constant.cardHeight

  const glob = document.documentElement.style
  for (c in constant)
    glob.setProperty("--" + c, constant[c])
}
