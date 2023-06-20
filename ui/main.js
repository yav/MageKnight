let sendJSON = null
let playerId = null
let gui      = null
let initialized = false

function main() {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) { html.setScale(conn.size) }
  setupConstants()
}

// Redraw the whole state
function uiRedraw(state) {

  if (!initialized) {
    gui = {}
    gui.question_cleanup = []

    document.getElementById("panel").style.backgroundImage =
      "url(\"../img/characters/" + state.game.playerHero + "/art.png\""

    gui.cards     = newCards()
    gui.hand      = newHand()
    gui.source    = newSource()
    gui.manaPool  = newManaPool()
    gui.map       = newMap()
    gui.phase     = newPhase()
    gui.combat    = newCombat()
    gui.resources = newResources()
    initialized   = true
  }


  uiUpdate(state.game)
  uiQuestions(state.questions)
}

// Perform a partial update
function uiUpdate(state) {

  gui.source.set(state._source)
  gui.phase.set(state._phase)
  gui.hand.set(state._hand)
  gui.manaPool.set(state._mana)
  gui.map.set(state._land.landMap)
  gui.map.setPlayer(state.playerHero, state._land.landPlayer)
  gui.resources(state)

}


