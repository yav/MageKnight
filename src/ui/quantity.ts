import { uiFromTemplate, uiFromTemplateNested } from "./common-js/template.ts"

/*
export
function newSetElement<T>(makeNew: (k:string, v:T) => T,
                          adaptor: (xs: {[key:string]: T} => {) {
  let cur = {}

  return (xs0: { [key: string]: T} ) => {
    const done = {}

    const xs = adaptor? adaptor(xs0) : xs0

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
      cur[key](undefined)
      delete cur[key]
    }
  }
}*/


export
type TextQunatity<T> = {
  dom: HTMLElement,
  set: (x: T) => void
}


export
function newQuantityText<T>() : TextQunatity<T> {
  let val: T | null = null
  const dom = uiFromTemplate("quantity-text")

  function set(v: T) {
    if (v === val) return
    dom.textContent = String(v)
    val = v
  }

  return { dom: dom, set: set }
}

type GroupedQuantity = {
  dom: HTMLElement,
  set: (n: number) => void,
  get: () => number
}

export
function newQuantityGrouped(thing: HTMLElement): GroupedQuantity {
  const [dom, els] = uiFromTemplateNested("quantity-grouped")
  const counter = els.counter
  dom.classList.add("hidden")
  counter.classList.add("hidden")
  dom.appendChild(thing)

  let have: number = 0

  function set(need: number) {
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
        counter.textContent = String(need)
        dom.classList.remove("hidden")
        counter.classList.remove("hidden")
        break
    }
    have = need
  }

  return {
    dom: dom,
    set: set,
    get: () => have
  }

}
