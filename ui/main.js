let sendJSON = null
let playerId = null
let iconSize = 26
let gui      = null

function main() {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) iconSize = conn.size
}

// Redraw the whole state
function uiRedraw(state) {
  html.getBody().innerHTML = ""
  gui = {}
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
  console.log(q)
  hsInput({
  })(q.chChoice)
}


// Perform a partial update
function uiUpdate(state) {
}


