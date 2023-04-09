
function newMap() {
  const domMap = document.getElementById("map")
  const dim    = domMap.getBoundingClientRect()
  let glob_x = 0
  let glob_y = dim.height - constant.tileSize * 7/10

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

  function setTile(t,x,y) {
    let cols = tiles[x]
    if (cols === undefined) { cols = {}; tiles[x] = cols }
    let have = cols[y]
    if (have !== undefined) {
      if (have.tile === t) return
      have.dom.remove()
      cols[y] = undefined
    }
    [x_pos,y_pos] = tileLoc(x,y)

    const [tile,els]   = uiFromTemplateNested("map-tile")
    tile.classList.add(t.tileType)
    tile.classList.add("Name" + t.tileName)
    tile.style.left = x_pos + "px"
    tile.style.top  = y_pos + "px"
    domMap.appendChild(tile)

    cols[y] = { tile: t, dom: tile }

    if (x_pos < 0) { glob_x = glob_x - x_pos }
    if (y_pos < 0) { glob_y = glob_y - y_pos }
    if (x_pos < 0 || y_pos < 0) shiftTiles()
  }

  const ex1 = { tileType: "CountryTile", tileName: "A" }
  const ex2 = { tileType: "CountryTile", tileName: "1" }
  setTile(ex1,0,0)
  setTile(ex1,1,0)
  setTile(ex1,2,0)
  setTile(ex1,3,0)
  setTile(ex1,0,1)

  const obj = {}
  return obj
}
