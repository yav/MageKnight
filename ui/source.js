function newSource() {
  const baseSize = 32

  const dom = html.div("source")
  html.setSize(dom,"height",baseSize)

  const mana = [ "Red", "Green", "White", "Blue", "Gold", "Black" ]

  const groups = {}

  function newDie(mana) {
    const d = html.img('img/mana/' + mana.toLowerCase() + '.png')
    d.classList.add('die')
    html.setDim(d,baseSize,baseSize)
    return d
  }

  function mkGroup(id,lab) {
    const g = html.div("group")
    g.setAttribute("title",lab)
    const qs = {}

    const n = mana.length
    for (let i = 0; i < n; ++i) {
      const m = mana[i]
      qs[m] = newQuantity(() => newDie(m))
      g.appendChild(qs[m].dom)
    }
    groups[id] = qs
    dom.appendChild(g)
    return g
  }

  mkGroup("sourceMana",  "Available mana")
  mkGroup("sourceFixed", "Used mana, not rerolled")
  mkGroup("sourceUsed",  "Used mana, rerolled")

  function set(s) {
    for(name in groups) {
      const amts = s[name]
      const g    = groups[name]
      const n = mana.length
      for (let i = 0; i < n; ++i) {
        const m  = mana[i]
        g[m].set(amts[m] || 0)
      }
    }
  }

  function ask(m,q) {
    const g = groups.sourceMana
    const it = g[m].get()
    uiExistingAnswer(g[m].get(),q)
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
