// Various things that can be used to answer the question.
function uiQuestion(q) {
  hsInput({
    Source: (m) => gui.source.ask(m,q),
    AskMana: (m) => gui.source.askMana(m,q),

    AskHand: (m) => gui.hand.ask(m,q),
    AskSelectedSideways: () => gui.hand.askSideways(q),
    AskSelectedAdvanced: () => gui.hand.askAdvanced(q),


    TestReroll: () => uiButton("Reroll",q),
    TestFixed: () => uiButton("Test fixed",q),
  })(q.chChoice)
}


function uiQuestionCleanup() {
  const todo = gui.question_cleanup
  const n = todo.length
  for (let i = 0; i < n; ++i) todo[i]()
  gui.question_cleanup = []
}

function uiAddQuestionCleanup(f) {
  gui.question_cleanup.push(f)
}

// Set the explanation for what we are asking.
function uiSetQuestion(q) {
  const dom = document.getElementById("question-box")
  dom.textContent = q
}

function uiNewAnswer(dom,q) {
  dom.addEventListener("click",() => {
    sendJSON(q)
    uiQuestionCleanup()
  })
  document.getElementById("resources").appendChild(dom)
  uiAddQuestionCleanup(() => dom.remove())
}

function uiExistingAnswer(dom,q) {
  dom.classList.add("question")
  const tit = dom.getAttribute("title")
  dom.setAttribute("title", q.chHelp)

  function onClick() { sendJSON(q); uiQuestionCleanup() }
  dom.addEventListener("click",onClick)
  uiAddQuestionCleanup(() => {
    dom.classList.remove("question")
    dom.removeEventListener("click",onClick)
    dom.setAttribute("title", tit)
  })
}

function uiUndo() { sendJSON( {tag: "undo"} ) }


