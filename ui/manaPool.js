function newManaPool () {
  const dom = document.getElementById("manaPool")

  const availMana = {}
  const availManaFromSource = {}

  for (let i = 0; i < mana.length; ++i) {
    const c = mana[i]
    const m  = uiFromTemplate("mana")
    const ms = uiFromTemplate("mana-from-source")
    m.classList.add(c)
    ms.classList.add(c)
    mana[c] = m
    availMana[c] = newQuantityGrouped(m)
    availManaFromSource[c] = newQuantityGrouped(ms)
    dom.appendChild(availMana[c].dom)
    dom.appendChild(availManaFromSource[c].dom)
  }

  function set(v) {
    const newM = v.mpMana
    const newS = v.mpSource

    function get(l,c) {
      const i = l[c]
      return i === undefined? 0 : i
    }

    for (let i = 0; i < mana.length; ++i) {
      const c = mana[i]
      availMana[c].set(get(newM,c))
      availManaFromSource[c].set(get(newS,c))
    }
  }

  const obj = {}
  obj.set = set
  return obj
}

