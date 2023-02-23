function newSource() {
  const baseSize = 32

  const dom = html.div("source")
  html.setSize(dom,"height",baseSize)

  const mana = [ "Red", "Green", "White", "Blue", "Gold", "Black" ]

  function mkGroup(x) {
    const g = html.div("group")
    g.setAttribute("title",x)
    return g
  }

  const groups = { sourceMana: {}, sourceFixed: {}, sourceUsed: {} }
  const cont = { sourceMana:  mkGroup("Available mana")
               , sourceFixed: mkGroup("Used mana, not rerolled")
               , sourceUsed:  mkGroup("Used mana, rerolled")
               }
  dom.appendChild(cont.sourceMana)
  dom.appendChild(cont.sourceFixed)
  dom.appendChild(cont.sourceUsed)

  function newDie(mana) {
    const d = html.img('img/mana/' + mana.toLowerCase() + '.png')
    d.classList.add('die')
    html.setDim(d,baseSize,baseSize)
    return d
  }

  function setGroup(name, allAmts) {
    const amts = allAmts[name]
    const g = groups[name]
    const c = cont[name]
    const n = mana.length
    for (let i = 0; i < n; ++i) {
      const m    = mana[i]
      const have = g[m] || []
      g[m]       = have
      const need = amts[m] || 0

      const diff = have.length - need
      for (let j = 0; j < diff; ++j) {
        have.pop().remove()
      }
      for (let j = diff; j < 0; ++j) {
        const d = newDie(m)
        if (name !== "sourceMana") html.setDim(d,baseSize*3/4,baseSize*3/4)
        have.push(d)
        c.appendChild(d)
      }
    }
  }

  function set(s) {
    lastMember = null
    for(c in groups) setGroup(c,s)
  }

  function ask(m,q) {
    const g = groups.sourceMana
    const have = g[m] || []
    if (have.length === 0) return

    const it = have[0]
    it.classList.add("question")
    const tit = it.getAttribute("title")
    it.setAttribute("title", q.chHelp)

    function onClick() { sendJSON(q); uiQuestionCleanup() }
    it.addEventListener("click",onClick)
    uiAddQuestionCleanup(() => {
      it.classList.remove("question")
      it.removeEventListener("click",onClick)
      it.setAttribute("title", tit)
    })
  }

  // Not directly related to source, but we have the die drawing code here
  function askMana(m,q) {
    const d = newDie(m)
    d.setAttribute("title",q.chHelp)
    d.classList.add("question")
    uiNewAnswer(d,q)
  }


  gui.container.appendChild(dom)

  const obj = {}
  obj.set = set
  obj.ask = ask
  obj.askMana = askMana

  return obj
}
