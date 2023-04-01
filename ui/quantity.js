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
