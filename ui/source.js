function newSource() {
  const baseSize = 32

  const mana = [ "Red", "Green", "White", "Blue", "Gold", "Black" ]

  const groups = {}

  function newDie(mana) {
    const dom = uiFromTemplate("die")
    dom.classList.add(mana)
    return dom
  }

  function mkGroup(id) {
    const g = document.getElementById(id)
    const qs = {}

    const n = mana.length
    for (let i = 0; i < n; ++i) {
      const m = mana[i]
      qs[m] = newQuantity(() => newDie(m))
      g.appendChild(qs[m].dom)
    }
    groups[id] = qs
    return g
  }

  mkGroup("sourceMana")
  mkGroup("sourceFixed")
  mkGroup("sourceUsed")

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


  const obj = {}
  obj.set = set
  obj.ask = ask
  obj.askMana = askMana

  return obj
}
