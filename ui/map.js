
function newMap() {
  const domMap = document.getElementById("map")
  const dim    = domMap.getBoundingClientRect()
  let glob_x = 0
  let glob_y = 0

  const tiles = {}

  function tileLoc(x,y) {
    const x_pos = glob_x + constant.tileSize * (x * 4/6 - y * 1/6)
    const y_pos = glob_y + constant.tileSize * -(x * 6/10 + y * 9/10)
    return [x_pos,y_pos]
  }

  function shiftTiles() {
    for (x in tiles) {
      const row = tiles[x]
      for (y in row) {
        [pos_x,pos_y] = tileLoc(x,y)
        const style = row[y].dom.style
        style.left = pos_x + "px"
        style.top  = pos_y + "px"
      }
    }
  }

  // XXX: Drawing of multiple enemies
  // Currently they'd end up on top of each other
  // XXX: Tool tip should go with hex, not with enemy
  function newEnemySet(hex) {
    const have = {}

    function enemyId(e) {
      switch (e.tag) {
        case "VisibleEnemy": return e.contents.enemyName
        default: return e.contents
      }
    }

    function enemyDom(e) {
      switch (e.tag) {
        case "VisibleEnemy": return newEnemy(e.contents)
        default:
          const dom = uiFromTemplate("enemy")
          const url = "img/enemies/" + e.contents + "/Type.jpg"
          dom.style.backgroundImage = "url(\"" + url + "\")"
          return dom
      }
    }

    return (need) => {
      const done = {}

      for (let i = 0; i < need.length; ++i) {
        const [e,n] = need[i]
        const nm = enemyId(e)
        done[nm] = true
        let haveE = have[nm]
        if (haveE === undefined) {
          const dom = enemyDom(e)
          haveE = newQuantityGrouped(dom)
          have[nm] = haveE
          hex.appendChild(haveE.dom)
        }
        haveE.set(n)
      }

      for (const nm in have) {
        if (done[nm]) continue
        have.dom.remove()
        have[nm] = undefined
      }
    }
  }


  function newShields(hex) {
    let have = false
    let dom  = null

    return (need) => {
      if (have) {
        if (!need) dom.remove()
      } else {
        if (need) {
          dom = undefined // XXX: draw shield
          hex.appendChild(dom)
        }
      }
      have = need
    }
  }

  // XXX
  function newRuins(hex) {
    return (need) => {
    }
  }


  function setContent(x,y,c) {
    const gt  = tiles[x][y]
    const cur = gt.content

    for (let li = 0; li < hex_locs.length; ++li) {
      const l = hex_locs[li]
      let have   = cur[l]
      const need = c[l]
      const dom  = gt.els[l]

      if (need === undefined) {
        if (have === undefined) continue

        // unlikely, tiles shouldn't disappear
        dom.innerHTML = ""
        have[l] = undefined
        continue
      }

      if (have === undefined) {
        have = { hRuins:   newRuins(dom)
               , hShield:  newShields(dom)
               , hEnemies: newEnemySet(dom)
               }
        cur[l] = have
      }
      have.hShield(need.hShield)
      have.hRuins(need.hShield)
      have.hEnemies(need.hEnemies)
    }
  }

  function hexHelp(help,info) {
    uiHelpHeader(help,undefined,info.hexTerrain)
    const ref = "img/rules/locations.jpg"
    const bigw  = 5600
    const bigh  = 7000
    const s     = 6

    const size  = bigw / s
    const w     = bigw / s / 4
    const h     = bigh / s / 7

    const mineH  = 1 / 2.7

    const features =
      { "Orc":              [ 0, 0 ]
      , "Draconum":         [ 1, 0 ]
      , "Monastery":        [ 2, 0 ]
      , "Village":          [ 3, 0 ]

      , "Walls":            [ 0, 1 ]
      , "AncientRuins":     [ 1, 1 ]

      , "MagicalGlade":     [ 2, 1 + mineH, h * (1 - mineH) ]

      , "Mine.Red":         [ 2, 1, h * mineH ]
      , "Mine.Green":       [ 2, 1, h * mineH ]
      , "Mine.Blue":        [ 2, 1, h * mineH ]
      , "Mine.White":       [ 2, 1, h * mineH ]
      // Deep Mines + refugee camp

      , "Dungeon":          [ 0, 2 ]
      , "Tomb":             [ 1, 2 ]
      , "MageTower":        [ 2, 2 ]
      , "Keep":             [ 3, 2 ]

      // Maze, Labyrinth
      , "MonsterDen":       [ 2, 3 ]
      , "SpawningGrounds":  [ 3, 3 ]

      // 4: NPC

      , "City.Green":       [ 0, 5 ]
      , "City.White":       [ 1, 5 ]
      , "City.Red":         [ 2, 5 ]
      , "City.Blue":        [ 3, 5 ]

      // XXX: [1,6]: city
      }

      const coord = features[info.hexFeature]
      if (coord) {
        const [x,y,hi] = coord
        uiHelpEntry(help,ref,size,-x * w,-y * h, w, hi ? hi : h)
      }
  }

  function setTile(x,y,gt) {
    const t       = gt.gameTile
    const content = gt.gameTileContent

    let cols = tiles[x]
    if (cols === undefined) { cols = {}; tiles[x] = cols }
    let have = cols[y]
    if (have !== undefined) {
      if (have.tile.tileType === t.tileType &&
          have.tile.tileName === t.tileName)
          return setContent(x,y,t.content)
      have.dom.remove()
      cols[y] = undefined
    }
    [x_pos,y_pos] = tileLoc(x,y)

    const [tile,els] = uiFromTemplateNested("map-tile")
    const url = "img/map-tiles/" + t.tileType + "/" + t.tileName + ".png"
    const style = tile.style
    style.backgroundImage = "url(\"" + url + "\")"
    style.left = x_pos + "px"
    style.top  = y_pos + "px"
    domMap.appendChild(tile)

    for (let i = 0; i < hex_locs.length; ++i) {
      const l     = hex_locs[i]
      const hex   = els[l]
      const help  = els[l + "-help"]
      hexHelp(help,t.tileTerrainMap[l])
      hex.addEventListener("mouseenter", () => {
        help.classList.remove("hidden")
      })
      hex.addEventListener("mouseleave", () => help.classList.add("hidden"))
    }

    cols[y] = { tile: t, dom: tile, els: els, content: {} }

    if (x_pos < 0) { glob_x = glob_x - x_pos }
    if (y_pos < 0) { glob_y = glob_y - y_pos }
    if (x_pos < 0 || y_pos < 0) shiftTiles()

    setContent(x,y,content)
  }

  function setMap(m) {
    const n = m.length
    for (let i = 0; i < n; ++i) {
      const [[x,y],val] = m[i]
      setTile(x,y,val)
    }
  }

  const obj = {}
  obj.set = setMap
  return obj
}
