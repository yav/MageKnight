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
        // XXX: selected damage if any
    }
  }

  function evKey(eid,mbAtt) {
    const front = eid.toString()
    if (mbAtt === null) return front
    return front + "_" + mbAtt.toString()
  }

  function drawActiveEnemy(container,ev) {
    const e = ev.evEnemy
    const a = ev.evAttack

    const dom = newEnemy(e.enemyToken, a)
    container.appendChild(dom)
    let cur = [ e.enemyToken.enemyName, a ]

    function set(val) {
      if (val === undefined) {
        dom.remove()
        return
      }
      const og = val.evEnemy.enemyToken
      const at = val.evAttack
      if (og.enemyName === cur[0] && at === cur[1]) return
      dom.replaceWith(newEnemy(og,at))
      cur = [ og.enemyName, at ]
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

    function ask(eid,mbAtt,q) {
      const obj = state[evKey(eid,mbAtt)]
      if (obj === undefined) return
      obj.ask(q)
    }

   function toObj(xs) {
     const res = {}
     for (let i = 0; i < xs.length; ++i) {
      const ev = xs[i]
      res[evKey(ev.evId, ev.evAttack)] = ev
     }
     return res
   }

    return { set: newSetElement(mk, toObj)
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
  }

  function askEnemy(eid,mbAtt,q) {
    selectedEnemies.ask(eid,mbAtt,q)
    notSelectedEnemies.ask(eid,mbAtt,q)
  }

  return {
    set: setCombat,
    hide: () => combatD.classList.add("hidden"),
    askEnemy: askEnemy
  }
}
