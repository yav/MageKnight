let sendJSON = null
let playerId = null
let gui      = null

function main() {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) { html.setScale(conn.size) }
}

// Redraw the whole state
function uiRedraw(state) {
  gui = {}
  gui.container = html.getBody()
  gui.container.innerHTML = ""
  gui.question_cleanup = []

  gui.cards   = newCards()
  gui.hand    = newHand()
  gui.source  = newSource()

  const ui_q = html.div("ui-question")
  gui.question_text    = html.div("description")
  gui.question_answers = html.div("answers")
  ui_q.appendChild(gui.question_text)
  ui_q.appendChild(gui.question_answers)
  gui.container.appendChild(ui_q)

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

  console.log("Scale = " + html.getVar("scale"))

  const dom = html.div("")
  dom.style.display = "flex"
  dom.style.flexDirection = "row"
  gui.container.appendChild(dom)

  gui.source.set(state._source)
  gui.hand.set(state._hand)


  //const es = state._enemies
  //for (let i = 0; i < es.length; ++i)
  //  newEnemy(state._enemies[i])

  //const ds = state._deeds
  //for (let i = 0; i < ds.length; ++i) {
  //  gui.container.appendChild(gui.cards.drawDeed(state._deeds[i]))
  //}


}


