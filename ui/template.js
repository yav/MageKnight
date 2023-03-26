
function uiFromTemplate(id) {
  const dom = document.getElementById(id).cloneNode(true)
  dom.removeAttribute("id")
  return dom
}


function uiFromTemplateNested(id) {
  const dom = uiFromTemplate(id)
  const els = {}
  function search(it) {
    for (n in it.children) {
      const a = n.getAttribute("id")
      if (a !== undefined) {
        els[a] = n
        n.removeAttribute("id")
      }
      search(n)
    }
  }
  return { dom: dom, nested: els }
}

