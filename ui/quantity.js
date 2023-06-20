
function newListElement(makeNew) {
  let old = []

  function set(xs) {
    const common = Math.min(old.length,xs.length)
    let i
    for (i = 0; i < common.length; ++i) {
      old[i](xs[i])
    }

    for (; i < xs.length; ++i) {
      old.push(makeNew(xs[i]))
    }

    while (i < old.length) old.pop()()
  }

  return set
}


function newQuantityText() {
  let val = null
  const dom = uiFromTemplate("quantity-text")

  function set(v) {
    if (v === val) return
    dom.textContent = v.toString()
    val = v
  }

  return { dom: dom, set: set }
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
  obj.get = () => have

  return obj
}
