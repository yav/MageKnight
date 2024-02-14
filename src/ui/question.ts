import { uiGet, uiFromTemplate } from "./common-js/template.ts"
import { Question } from "./common-js/connect.ts"
import { conn, gui } from "./main.ts"


// XXX: we should generate these
export
type Q =
    { tag: "Source", contents: any[] }
  | { tag: "AskMana", contents: any[] }
  | { tag: "AskManaPool", contents: any[] }
  | { tag: "AskHand", contents: any[] }
  | { tag: "AskSelectedSideways", contents: any[] }
  | { tag: "AskSelectedAdvanced", contents: any[] }
  | { tag: "AskLoc", contents: any[] }
  | { tag: "AskEnemy", contents: any[] }
  | { tag: "ActionButton", contents: any[] }
  | { tag: "AskText", contents: any[] }
  | { tag: "TestReroll", contents: any[] }
  | { tag: "TestFixed", contents: any[] }


// Various things that can be used to answer the question.
export
function uiQuestion(q: Question<Q>) {
  const val = q.chChoice
  const vals = val.contents
  const m = vals[0]
  const n = vals[1]
  switch (val.tag) {
    case "Source": return gui.source.ask(m,q)
    case "AskMana": return gui.source.askMana(m,q)
    case "AskManaPool": return gui.manaPool.askMana(m,q)

    case "AskHand": return gui.hand.ask(m,q)
    case "AskSelectedSideways": return gui.hand.askSideways(q)
    case "AskSelectedAdvanced": return gui.hand.askAdvanced(q)

    case "AskLoc": return gui.map.ask(m,n,q)

    case "AskEnemy": return gui.combat.askEnemy(m,n,q)

    case "ActionButton": return uiActionButton(m,q)
    case "AskText": return uiButton(m,q)
    case "TestReroll": return uiButton("Reroll",q)
    case "TestFixed": return uiButton("Test fixed",q)
  }
}

export
function uiQuestionCleanup() {
  const todo = gui.question_cleanup
  const n = todo.length
  for (let i = 0; i < n; ++i) todo[i]()
  gui.question_cleanup = []
}

export
function uiAddQuestionCleanup(f) {
  gui.question_cleanup.push(f)
}

// Set the explanation for what we are asking.
export
function uiSetQuestion(q) {
  const dom = uiGet("question-box")
  dom.textContent = q
}

export
function uiNewAnswer(dom: HTMLElement,q: Question<Q>) {
  dom.addEventListener("click",() => {
    conn.sendJSON(q)
    uiQuestionCleanup()
  })
  uiGet("resources").appendChild(dom)
  uiAddQuestionCleanup(() => dom.remove())
}

export
function uiExistingAnswer(dom: HTMLElement, q: Question<Q>) {
  dom.classList.add("question")
  const tit = dom.getAttribute("title")
  dom.setAttribute("title", q.chHelp)

  function onClick() { conn.sendJSON(q); uiQuestionCleanup() }
  dom.addEventListener("click",onClick)
  uiAddQuestionCleanup(() => {
    dom.classList.remove("question")
    dom.removeEventListener("click",onClick)
    if (tit) dom.setAttribute("title", tit)
         else dom.removeAttribute("title")
  })
}

function uiUndo() {
  uiQuestionCleanup()
  conn.sendJSON( {tag: "undo"} )
}

function uiActionButton(msg: string, q: Question<Q>) {
  const dom = uiGet("multi-button")
  dom.textContent = msg
  dom.classList.remove("hidden")
  uiExistingAnswer(dom,q)
  uiAddQuestionCleanup(() => dom.classList.add("hidden"))
}

function uiButton(lab: string, q: Question<Q>) {
  const dom = uiFromTemplate("button")
  dom.textContent = lab
  uiNewAnswer(dom,q)
}



