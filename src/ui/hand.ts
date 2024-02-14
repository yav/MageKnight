import { gui, conn } from "./main.ts"
import { Question } from "./common-js/connect.ts"
import { uiFromTemplate, uiGet } from "./common-js/template.ts"
import { Q, uiExistingAnswer, uiAddQuestionCleanup, uiQuestionCleanup } from "./question.ts"
import { SelectorBoxes } from "./cards.ts"


export
type SelectedMode = "SelectedBasic" | "SelectedAdvanced" | "SelectedSideways"

export
type Selection = {
  _selectedDeed: string,
  _selectedMode: SelectedMode
}

export
type Hand = {
  _handSelected: Selection | null,
  _handCards: string[]
}

export
type HandUI = {
  set: (x: Hand) => void,
  select:  (x: Selection | null) => void,
  ask: (ix: number,q: Question<Q>) => void,
  askAdvanced: (q: Question<Q>) => void,
  askSideways: (q: Question<Q>) => void
}

export
function newHand(): HandUI {

  const dcards    = uiGet("hand")
  const dpreview  = uiGet("card-preview")
  const dselected = uiGet("card-selected")

  const getName   = gui.cards.getName
  const draw      = (d: string) => gui.cards.drawDeed(d)

  let selected: Selected | null = null

  type Selected = {
    name: string, 
    dom: HTMLElement,
    selectors: SelectorBoxes,
    mode: SelectedMode | null
  }

  function setSelected(newSel: Selection | null) {

    if (newSel === null) {
      if (selected === null) return
      selected.dom.remove()
      selected = null
      return
    }

    const newDeed = newSel._selectedDeed
    const newMode = newSel._selectedMode

    const name = getName(newDeed)
    if (selected !== null && selected.name === name) setMode(newMode)

    const drowF = draw(newDeed)
    const dom = drowF.big()

    if (selected !== null) selected.dom.replaceWith(dom)
    else dselected.appendChild(dom)
    selected = { name: name, dom: dom
               , selectors: drowF.selectors(dom)
               , mode: null
               }
    setMode(newMode)
  }

  function setMode(newMode: SelectedMode) {
    if (selected === null) return
    if (newMode === selected.mode) return
    switch (newMode) {
      case "SelectedBasic":
        selected.dom.classList.remove("sideways")
        selected.selectors.hide(0)
        selected.selectors.show(1)
        break
      case "SelectedAdvanced":
        selected.dom.classList.remove("sideways")
        selected.selectors.show(0)
        selected.selectors.hide(1)
         break
      case "SelectedSideways":
        selected.dom.classList.add("sideways")
        selected.selectors.hide(0)
        selected.selectors.hide(1)
        break
    }
    selected.mode = newMode
  }



  const deeds: DrawnDeed[] = []
  type DrawnDeed = {
    name: string, small: HTMLElement, big: HTMLElement
  }



  function setHand(hand: Hand) {

    setSelected(hand._handSelected)

    const newDeeds = hand._handCards

    function newHandCard(d: string,i: number,replace: boolean) {
      const funs  = draw(d)
      const small = funs.small()
      const big   = funs.big()
      const bigCS = big.classList
      bigCS.add("hidden")
      small.addEventListener("mouseenter", () => bigCS.remove("hidden"))
      small.addEventListener("mouseleave", () => bigCS.add("hidden"))

      if (!replace) {
        dcards.appendChild(small)
        dpreview.appendChild(big)
      } else {
        deeds[i].small.replaceWith(small)
        deeds[i].big.replaceWith(big)
      }
      deeds[i] = { name: funs.name, small: small, big: big }

    }

    const common = Math.min(deeds.length,newDeeds.length)
    let i: number
    for (i = 0; i < common; ++i) {
      const d  = deeds[i]
      const nd = newDeeds[i]
      const newName = getName(nd)
      if (d.name === newName) continue
      newHandCard(nd,i,true)
    }

    for(; i < deeds.length; ++i) {
      const it = deeds.pop()
      if (it === undefined) continue
      it.small.remove()
      it.big.remove()
    }

    for (; i < newDeeds.length; ++i) {
      newHandCard(newDeeds[i],i,false)
    }

  }


  function askHand(ix: number,q: Question<Q>) {
    uiExistingAnswer(deeds[ix].small,q)
  }

  function askSideways(q: Question<Q>) {
    if (selected === null) return
    const dom = uiFromTemplate("card-sideways-icon")
    selected.dom.appendChild(dom)
    uiAddQuestionCleanup(() => dom.remove())
    dom.addEventListener("click",() => {
      conn.sendJSON(q)
      uiQuestionCleanup()
    })
  }

  function askAdvanced(q: Question<Q>) {
    if (selected === null) return
    uiExistingAnswer(selected.selectors.get(1), q)
  }

  return {
    set: setHand,
    select: setSelected,
    ask: askHand,
    askAdvanced: askAdvanced,
    askSideways: askSideways
  }
}


