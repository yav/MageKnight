
export
function uiGet(id: string) : HTMLElement {
  const d = document.getElementById(id)
  if (d === null) { throw new Error("Missing id: " + id) }
  return d
}

export
function uiFromTemplate(id: string) : HTMLElement {
  const dom = uiGet(id).cloneNode(true) as HTMLElement
  dom.removeAttribute("id")
  return dom
}

export
function uiFromTemplateNested(id: string)
  : [ HTMLElement, { [domId:string]: HTMLElement } ] {
  const dom = uiFromTemplate(id)
  const els = {}
  function search(it: HTMLElement) {
    for (const el of it.children) {
      const a = el.getAttribute("id")
      if (a !== null) {
        els[a] = el
        el.removeAttribute("id")
      }
      if (el instanceof HTMLElement) { search(el) }
    }
  }
  search(dom)
  return [ dom, els ]
}

