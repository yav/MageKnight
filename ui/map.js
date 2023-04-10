
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

  function setContent(x,y,c) {
    const cur = tiles[x][y].content
    for (let li = 0; li < hex_locs.length; ++li) {
      const l = hex_locs[li]
      const have = cur[l]
      const need = c[l]
      if (have !== undefined &&
          need !== undefined &&
          have.name === need.name ||
          have === undefined &&
          need === undefined) continue

      if (have !== undefined) have.dom.remove()
      if (need === undefined) {
        cur[l] = undefined
        continue
      }
      console.log(need)
      const dom = uiFromTemplate("character")
      const url = "img/characters/" + need + "/figure.png"
      dom.style.backgroundImage = "url(\"" + url + "\")"
      tiles[x][y].els[l].appendChild(dom)
      cur[l] = { name: need, dom: dom }
    }
  }

  function setTile(x,y,t) {
    let cols = tiles[x]
    if (cols === undefined) { cols = {}; tiles[x] = cols }
    let have = cols[y]
    if (have !== undefined) {
      if (have.tile === t) return setContent(x,y,t.content)
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

    cols[y] = { tile: t, dom: tile, els: els, content: {} }

    if (x_pos < 0) { glob_x = glob_x - x_pos }
    if (y_pos < 0) { glob_y = glob_y - y_pos }
    if (x_pos < 0 || y_pos < 0) shiftTiles()

    setContent(x,y,t.content)
  }

  const ex1 = { tileType: "CountryTile", tileName: "A" }
  for (let x = 0; x < 3; ++x) {
    for (let y = 0; y < 3; ++y) {
      const ex2 = { tileType: "CountryTile"
                  , tileName: 1 + (x + y) % 9
                  , content:
                      { NW: "Arythea"
                      , Center: "Goldyx"
                      , SW: "Tovak"
                      , E: "Norowas"
                      }
                  }
      setTile(x,y,ex2)
    }
  }

  const obj = {}
  return obj
}
