
function newEnemy(e) {
  console.log(e)
  const baseSize = 96
  const url = "img/enemies/" + e.enemyType + "/" + e.enemyName + ".jpg"

  const dom = html.div("enemy")
  html.setDim(dom,baseSize,baseSize)
  dom.style.backgroundImage = "url(\"" + url + "\")"


  const help = html.div("help hidden")
  html.setSize(help,"left", 0.75 * baseSize )
  dom.appendChild(help)

  function entry(x) {
    const e = html.div("item")
    e.textContent = x
    help.appendChild(e)
  }

  const name = html.div("item name")
  name.textContent = e.enemyName
  help.appendChild(name)

  const as = e.enemyAbilities
  for (let i = 0; i < as.length; ++i)
    entry(JSON.stringify(as[i]))


  dom.addEventListener("mouseenter", () => help.classList.remove("hidden"))
  dom.addEventListener("mouseleave", () => help.classList.add("hidden"))

  gui.container.appendChild(dom)
}
