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
  gui.source = newSource()
  uiUpdate(state.game)
  uiQuestions(state.questions)
}

// Set the explanation for what we are asking.
function uiSetQuestion(q) {
  console.log(q)
}

// Various things that can be used to answer the question.
function uiQuestion(q) {
  hsInput({
    Source: (m) => gui.source.ask(m,q)
  })(q.chChoice)
}


// Perform a partial update
function uiUpdate(state) {
  console.log("update")
  gui.source.set(state[1])
}


