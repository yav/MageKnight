function newPhase() {

  return { set: (ph) => {
    const content = ph.contents
    switch(ph.tag) {
      case "ActionPhase":
        switch (content.tag) {
          case "CombatAction":
            gui.combat.set(content.contents)
            return
        }
    }
    gui.combat.hide()
  }}
}


function newCombat() {

  const combatD      = document.getElementById("combat")
  const phaseD       = document.getElementById("combat-phase")
  const deedsD       = document.getElementById("combat-deeds")
  const selectedD    = document.getElementById("combat-enemies-selected")
  const unselectedD  = document.getElementById("combat-enemies-not-selected")
  const woundsD      = document.getElementById("combat-wounds")

  const phaseName   = newQuantityText()
  const have        = newQuantityText()
  const need        = newQuantityText()
  phaseD.appendChild(phaseName.dom)
  phaseD.append(": ")
  phaseD.appendChild(have.dom)
  phaseD.append("/")
  phaseD.appendChild(need.dom)
  // XXX: wounds


  function setPhase(ph) {
    const content = ph.contents
    switch (ph.tag) {
      case "RangedAttackView":
        phaseName.set("Ranged Attack")
        have.set(content[0])
        need.set(content[1])
        break
      case "BlockView":
        phaseName.set("Block")
        have.set(content[0])
        need.set(content[1])
        break
      case "AttackView":
        phaseName.set("Attack")
        have.set(content[0])
        need.set(content[1])
        break
      case "DamageView":
        phaseName.set("Assign Damage")
    }
  }


  function drawActiveEnemy(container,e) {
    const dom = newEnemy(e.enemyToken)
    container.appendChild(dom)
    let cur = e.enemyToken.enemyName

    function set(val) {
      if (val === undefined) {
        dom.remove()
        return
      }
      const og = e.enemyToken
      if (og.enemyName === cur) return
      dom.replaceWith(newEnemy(og))
      cur = og.enemyName
    }

    function ask(q) {
      uiExistingAnswer(dom,q)
    }

    return { set: set, ask: ask }
  }

  function newEnemyList (container) {
    const state = {}

    function mk(eid,enemyVal) {
      state[eid] = drawActiveEnemy(container, enemyVal)
      return (newVal) => {
        state[eid].set(newVal)
        if (newVal === undefined) delete state[eid]
      }
    }

    function ask(eid,q) {
      const obj = state[eid]
      if (obj === undefined) return
      obj.ask(q)
    }

    return { set: newSetElement(mk, fromKeyValArray)
           , ask: ask
           }

  }

  const selectedEnemies = newEnemyList(selectedD)
  const notSelectedEnemies = newEnemyList(unselectedD)

  function setCombat(c) {
    combatD.classList.remove("hidden")
    setPhase(c.viewBattlePhase)
    selectedEnemies.set(c.viewEnemiesSelected)
    notSelectedEnemies.set(c.viewEnemiesUnselected)
    // XXX: Deeds
  }

  function askEnemy(eid,q) {
    selectedEnemies.ask(eid,q)
    notSelectedEnemies.ask(eid,q)
  }

  return {
    set: setCombat,
    hide: () => combatD.classList.add("hidden"),
    askEnemy: askEnemy
  }
}
