import { uiFromTemplate, uiGet } from "./common-js/template.ts"
import { Mana, mana } from "./constants.ts"
import { uiExistingAnswer, uiNewAnswer } from "./question.ts"
import { newQuantityGrouped } from "./quantity.ts"
import { Question } from "./common-js/connect.ts"
import { Q } from "./question.ts"


export
type SourceUI = {
  set: (s: SourceMap) => void,
  ask: (m: Mana, q: Question<Q>) => void,
  askMana: (m: Mana, q: Question<Q>) => void
}

export
type SourceMap = { sourceMana: number, sourceFiex: number, sourceUsed: number }

export
function newSource(): SourceUI {

  function newDie(mana: Mana): HTMLElement {
    const dom = uiFromTemplate("die")
    dom.classList.add(mana)
    return dom
  }

  function mkGroup(id: string): HTMLElement {
    const g = uiGet(id)
    const qs = {}

    const n = mana.length
    for (let i = 0; i < n; ++i) {
      const m = mana[i]
      qs[m] = newQuantityGrouped(newDie(m))
      g.appendChild(qs[m].dom)
    }
    return g
  }

  const groups = {
    sourceMana: mkGroup("sourceMana"),
    sourceFixed: mkGroup("sourceFixed"),
    sourceUsed: mkGroup("sourceUsed")
  }
  

  function set(s: SourceMap) {
    for(const name in groups) {
      const amts = s[name]
      const g    = groups[name]
      const n = mana.length
      for (let i = 0; i < n; ++i) {
        const m  = mana[i]
        g[m].set(amts[m] || 0)
      }
    }
  }

  function ask(m: Mana, q: Question<Q>) {
    const g = groups.sourceMana
    uiExistingAnswer(g[m].dom,q)
  }

  // Not directly related to source, but we have the die drawing code here
  function askMana(m: Mana, q: Question<Q>) {
    const d = newDie(m)
    d.setAttribute("title",q.chHelp)
    d.classList.add("question")
    uiNewAnswer(d,q)
  }


  return {
    set: set,
    ask: ask,
    askMana: askMana
  }

}
