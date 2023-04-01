
function newHand() {

  const dcards    = document.getElementById("hand")
  const dpreview  = document.getElementById("card-preview")
  const dselected = document.getElementById("card-selected")

  const getName   = gui.cards.getName
  const draw      = (d) => gui.cards.drawDeed(d)

  let selected = null

  function setSelected(newSel) {

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

  function setMode(newMode) {
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



  const deeds = []

  function setHand(hand) {

    setSelected(hand._handSelected)

    const newDeeds = hand._handCards

    function newHandCard(d,i,replace) {
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
    let i
    for (i = 0; i < common; ++i) {
      const d  = deeds[i]
      const nd = newDeeds[i]
      const newName = getName(nd)
      if (d.name === newName) continue
      newHandCard(nd,i,true)
    }

    for(; i < deeds.length; ++i) {
      const it = deeds.pop()
      it.small.remove()
      it.big.remove()
    }

    for (; i < newDeeds.length; ++i) {
      newHandCard(newDeeds[i],i,false)
    }

  }


  function askHand(ix,q) {
    uiExistingAnswer(deeds[ix].small,q)
  }

  function askSideways(q) {
    if (selected === null) return
    const dom = uiFromTemplate("card-sideways-icon")
    selected.dom.appendChild(dom)
    uiAddQuestionCleanup(() => dom.remove())
    dom.addEventListener("click",() => {
      sendJSON(q)
      uiQuestionCleanup()
    })
  }

  function askAdvanced(q) {
    if (selected === null) return
    uiExistingAnswer(selected.selectors.get(1), q)
  }

  const obj = {}
  obj.set = setHand
  obj.select = setSelected
  obj.ask = askHand
  obj.askAdvanced = askAdvanced
  obj.askSideways = askSideways

  return obj
}


