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
  console.log("uiRedraw")

  if (!initialized) {
    gui = {}
    gui.question_cleanup = []

    gui.cards   = newCards()
    gui.hand    = newHand()
    gui.source  = newSource()
    initialized = true
  }


  uiUpdate(state.game)
  uiQuestions(state.questions)
}

function uiButton(lab,q) {
  const dom = html.div("button question")
  dom.textContent = lab
  uiNewAnswer(dom,q)
}


// Perform a partial update
function uiUpdate(state) {

  gui.source.set(state._source)
  gui.hand.set(state._hand)

}


