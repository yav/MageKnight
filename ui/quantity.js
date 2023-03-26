function newQuantity(group, mk) {
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

function newQuantityGrouped(thing) {
  const [dom, els] = uiFromTemplateNested("quantity-grouped")
  const counter = els.counter
  dom.classList.add("hidden")
  counter.classList.add("hidden")
  dom.appendChild(thing)

  let have = 0

  function set(need) {
    if (have === need) return
    have = need
    switch (need) {

      case 0:
        dom.classList.add("hidden")
        break

      case 1:
        dom.classList.remove("hidden")
        counter.classList.add("hidden")
        break

      default:
        counter.textContent = need
        dom.classList.remove("hidden")
        counter.classList.remove("hidden")
        break
    }
    have = need;
  }

  const obj = {}
  obj.dom = dom
  obj.set = set
  obj.get = () => dom

  return obj
}
