function newSetElement(makeNew, adaptor) {
  let cur = {}

  return (xs0) => {
    const done = {}

    const xs = adaptor? adaptor(xs0) : xs

    // Add nes and change existing
    for (const key in xs) {
      const val = xs[key]
      const obj = cur[key]
      if (obj === undefined) {
        cur[key] = makeNew(key,val)
      } else {
        obj(val)
      }
      done[key] = true
    }

    // Remove extras
    for (const key in cur) {
      if (done[key]) continue
      console.log(cur[key])
      cur[key](undefined)
      delete cur[key]
    }
  }
}

function fromArray(arr) {
  const obj = {}
  for (let i = 0; i < arr.length; ++i) obj[i] = arr[i]
  return obj
}

function fromKeyValArray(arr) {
  const obj = {}
  for (let i = 0; i < arr.length; ++i) {
    const [k,v] = arr[i]
    obj[k] = v
  }
  return obj
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
