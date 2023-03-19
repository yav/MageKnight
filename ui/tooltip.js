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

