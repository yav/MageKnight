
function newResources() {
  const move = newQuantityGrouped(uiFromTemplate("move-resource"))
  const cont = document.getElementById("resources")
  cont.appendChild(move.dom)
  return (state) => {
    move.set(state._movement)
  }
}
