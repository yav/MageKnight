import { srvConnect, Connection } from "./common-js/connect.ts"
import { uiGet } from "./common-js/template.ts"
import { setupConstants } from "./constants.ts"
import { Q, uiQuestion, uiSetQuestion } from "./question.ts"
import { newCards, CardsUI } from "./cards.ts"
import { newHand, HandUI } from "./hand.ts"
import { newSource, SourceUI } from "./source.ts"

export
let gui: {
  question_cleanup: (() => void)[],
  cards: CardsUI,
  hand: HandUI,
  source: Source,
  manaPool: any,
  map: any,
  phase: any,
  combat: any,
  resources: any

}

let initialized = false

export
let conn: Connection<Q>

export
function main() {
  const cb = {
    uiRedraw: uiRedraw,
    uiSetQuestion: uiSetQuestion,
    uiQuestion: uiQuestion,
    uiUpdate: uiUpdate
  }
  conn = srvConnect(cb)
  setupConstants()
}

// Redraw the whole state
function uiRedraw(state) {

  if (!initialized) {

    uiGet("panel").style.backgroundImage =
      "url(\"../img/characters/" + state.game.playerHero + "/art.png\""

    gui = {
      question_cleanup: [],
      cards     : newCards(),
      hand      : newHand(),
      source    : newSource(),
      manaPool  : newManaPool(),
      map       : newMap(),
      phase     : newPhase(),
      combat    : newCombat(),
      resources : newResources()
    }
    initialized   = true
  }


  uiUpdate(state.game)
  conn.uiQuestions(state.questions)
}

// Perform a partial update
function uiUpdate(state) {

  gui.source.set(state._source)
  gui.phase.set(state._phase)
  gui.hand.set(state._hand)
  gui.manaPool.set(state._mana)
  gui.map.set(state._land.landMap)
  gui.map.setPlayer(state.playerHero, state._land.landPlayer)
  gui.resources(state)

}


