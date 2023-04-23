function newTooltip(dom,help) {
  dom.addEventListener("mouseenter", () => {
    help.classList.remove("hidden")
    const w = window.innerWidth
    const h = window.innerHeight
    let dod = dom.getBoundingClientRect()

    let d = help.getBoundingClientRect()
    help.style.top = "0"
    help.style.left = dod.width + "px"
    if (d.right > w) {
        help.style.left = -d.width + "px"
    }
    d = help.getBoundingClientRect()
    let over = false
    if (d.left < 0) {
      help.style.left = -(d.width + d.left) + "px"
      html.setSize(help, "top", baseSize)
      over = true
    }
    d = help.getBoundingClientRect()
    if (d.bottom > h) {
      const extra = d.bottom - h
      help.style.top = (over ? -d.height : -extra) + "px"
    }
  })
  dom.addEventListener("mouseleave", () => help.classList.add("hidden"))
}


function uiHelpHeader (help, iconURL, title) {
  const name = html.div("item name")
  help.appendChild(name)

  if (iconURL !== undefined) {
    const icon = html.img(iconURL)
    icon.classList.add("icon")
    icon.style.width  = "1em"
    icon.style.height = "1em"
    name.appendChild(icon)
  }

  const lab = html.div("label")
  lab.textContent = title
  name.appendChild(lab)
}

function uiHelpEntry(help, url, size, x, y, w, h) {
  const e = html.div("item")
  e.style.backgroundImage = "url(\"" + url + "\")"
  html.setSize(e,"backgroundSize", size)
  html.setSize2(e,"backgroundPosition", x, y)
  html.setDim(e,w,h)
  help.appendChild(e)
}






