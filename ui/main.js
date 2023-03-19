let sendJSON = null
let playerId = null
let gui      = null

function main() {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) { html.setScale(conn.size) }
}

function uiQuestionCleanup() {
  const todo = gui.question_cleanup
  const n = todo.length
  for (let i = 0; i < n; ++i) todo[i]()
  gui.question_cleanup = []
}

function uiAddQuestionCleanup(f) { gui.question_cleanup.push(f) }

// Redraw the whole state
function uiRedraw(state) {
  gui = {}
  gui.container = html.getBody()
  gui.container.innerHTML = ""
  gui.question_cleanup = []

  gui.cards = newCards()

  const ui_q = html.div("ui-question")
  gui.question_text    = html.div("description")
  gui.question_answers = html.div("answers")
  ui_q.appendChild(gui.question_text)
  ui_q.appendChild(gui.question_answers)
  gui.container.appendChild(ui_q)

  gui.source = newSource()
  uiUpdate(state.game)
  uiQuestions(state.questions)
}

// Set the explanation for what we are asking.
function uiSetQuestion(q) {
  gui.question_text.textContent = q
}

function uiNewAnswer(dom,q) {
  dom.addEventListener("click",() => {
    sendJSON(q)
    uiQuestionCleanup()
  })
  gui.question_answers.appendChild(dom)
  uiAddQuestionCleanup(() => dom.remove())
}


// Various things that can be used to answer the question.
function uiQuestion(q) {
  hsInput({
    Source: (m) => gui.source.ask(m,q),
    AskMana: (m) => gui.source.askMana(m,q),
    TestReroll: () => uiButton("Reroll",q),
    TestFixed: () => uiButton("Test fixed",q),
  })(q.chChoice)
}

function uiButton(lab,q) {
  const dom = html.div("button question")
  dom.textContent = lab
  uiNewAnswer(dom,q)
}


// Perform a partial update
function uiUpdate(state) {

  const dom = html.div("")
  dom.style.display = "flex"
  dom.style.flexDirection = "row"
  gui.container.appendChild(dom)

  gui.source.set(state._source)
  const es = state._enemies
  for (let i = 0; i < es.length; ++i)
    newEnemy(state._enemies[i])

  const ds = state._deeds
  for (let i = 0; i < ds.length; ++i) {
    gui.container.appendChild(gui.cards.drawDeed(state._deeds[i]))
  }

  // gui.cards.drawSet("Wound")
  //gui.cards.drawSet("advanced")
  //gui.cards.drawSet("units_regular")
  //gui.cards.drawSet("units_elite")
  //gui.cards.drawSet("spells")
  //gui.cards.drawSet("artifacts")

}


