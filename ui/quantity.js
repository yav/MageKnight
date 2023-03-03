function newQuantity(mk) {
  const dom = html.div("quantity")

  let have = 0
  const els = []

  function set(need) {
    if (have === need) return
    const extra = have - need
    for (let i = 0; i < extra; ++i) els.pop().remove()
    for (let i = extra; i < 0; ++i) {
      const el = mk()
      els.push(el)
      dom.appendChild(el)
    }
    have = need
  }

  function get() { return els[0] }

  const obj = {}
  obj.dom = dom
  obj.set = set
  obj.get = get

  return obj
}
