let sendJSON = null
let playerId = null
let iconSize = 26
let gui      = null

const main = () => {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) iconSize = conn.size
}

// Redraw the whole state
const uiRedraw = (state) => {
  uiQuestions(state.questions)
}

// Set the explanation for what we are asking.
const uiSetQuestion = (q) => {
}

// Various things that can be used to answer the question.
const uiQuestion = (q) => hsInput({
  })(q.chChoice)


// Perform a partial update
const uiUpdate = hsUpdate({
  })


