function newSource() {
  const dom = html.div("sorce")

  function newDie(mana) {
    const d = html.img('img/mana/' + mana + '.png')
    d.classList.add('die')
    return d
  }

  const obj = {}
  obj.dom = dom

  return obj
}
