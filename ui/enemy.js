
function newEnemy(e) {
  const baseSize = 96
  const urlI = "img/enemies/" + e.enemyType + "/Type.jpg"
  const url = "img/enemies/" + e.enemyType + "/" + e.enemyName + ".jpg"

  const dom = html.div("enemy")
  html.setDim(dom,baseSize,baseSize)
  dom.style.backgroundImage = "url(\"" + url + "\")"


  const help = html.div("help hidden")
  html.setSize(help,"left", baseSize)
  html.setSize(help,"top", 0)
  dom.appendChild(help)

  function entry(x,y,h) {
    const e = html.div("item")
    e.style.backgroundImage = "url(\"img/rules/abilities.jpg\")"
    html.setSize(e,"backgroundSize", 800)
    html.setSize2(e,"backgroundPosition", x == 1 ? -35
                                        : x == 2 ? -290
                                        : -282
                                        , -y)
    html.setDim(e,235,h)
    help.appendChild(e)
  }

  const name = html.div("item name")
  help.appendChild(name)

  const icon = html.img(urlI)
  icon.classList.add("icon")
  icon.style.width  = "1em"
  icon.style.height = "1em"
  name.appendChild(icon)

  const lab = html.div("label")
  lab.textContent = e.enemyName
  name.appendChild(lab)

  const attacks = e.enemyAttack
  const attackLen = attacks.length
  if (attackLen > 1) {
    entry(2.5,660,100)
  }


  const doneAttack = {}
  for (let i = 0; i < attackLen; ++i) {
    const a = attacks[i]
    switch (a.tag) {
      case "Summoner":
        if (doneAttack.Summoner) continue;
        entry(1,255,60);
        doneAttack.Summoner = true;
        break

      case "AttacksWith":
        if (doneAttack[a.contents[0]]) continue

        switch (a.contents[0]) {
          case "Fire": entry(1,115,35); break
          case "Ice": entry(1,165,30); break
          case "ColdFire": entry(1,210,40); break
        }
        doneAttack[a.contents[0]] = true
    }
  }

  const as = e.enemyAbilities
  let fire = false
  let ice  = false
  let both = false
  for (let i = 0; i < as.length; ++i) {
    const a = as[i]
    switch (a.tag) {
      case "Swift": entry(1,320,40); break
      case "Brutal": entry(1,370,35); break
      case "Poisons": entry(1,410,90); break
      case "Paralyzes": entry(1,500,90); break
      case "Assassin": entry(1,590,50); break
      case "Cumbersome": entry(1,640,65); break
      case "Fortified": entry(2,115,45); break
      case "Resists":
        switch(a.contents) {

          case "Physical":
            entry(2,175,30)
            break

          case "Fire":
            entry(2,225,30)
            fire = true
            break
          case "Ice":
            entry(2,295,30)
            ice = true
            break
        }
        if (fire && ice && !both) {
          entry(2,365,35)
          both = true
        }
        break
      case "Elusive": entry(2,415,70); break;
      case "Unfortified": entry(2,495,35); break;
      case "ResistArcane": entry(2,545,45); break;
    }
  }

  let tit = "D:" + e.enemyArmor + ",A:"
  for (let i = 0; i < e.enemyAttack.length; ++i) {
    const a = e.enemyAttack[i]
    switch (a.tag) {
      case "Summoner": tit += "S,"; break
      default:
        tit += a.contents[1]
        switch(a.contents[0]) {
          case "Fire": tit += "F,"; break
          case "Ice": tit += "I,"; break
          case "ColdFire": tit += "CF,"; break
          case "Physical": tit += ","; break
        }
    }
  }

  const titD = html.div("item")
  tit += "F:" + e.enemyFameGain
  if (e.enemyReputationGain !== 0) {
    tit += ",R:" + e.enemyReputationGain
  }
  titD.textContent = tit
  help.appendChild(titD)

  newTooltip(dom,help)

  document.getElementById("rhs").appendChild(dom)

}
