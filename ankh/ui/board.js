function drawBoard(board) {
  console.log(board)

  const dom = html.div('board')

  let maxX = 0
  let maxY = 0

  const terrainInfo = board.hexes
  for (const xTxt in terrainInfo) {
    const x = parseInt(xTxt)
    const thisX = terrainInfo[xTxt]
    for (const yTxt in thisX) {
      const y = parseInt(yTxt)
      const info = thisX[yTxt]
      const terrain = info.terrain
      const contnet = info.contnet
      const h = hex(x,y,terrain)
      const loc = hexLoc(x,y)
      if (loc.x > maxX) maxX = loc.x
      if (loc.y > maxY) maxY = loc.y
      dom.appendChild(h)
    }
  }

  const borders = board.borders
  for (const b in borders) {
    const wall = borders[b]
    const h    = wall[0]
    const w    = hexWall(h[0],h[1],wall[1])
    dom.appendChild(w)
  }

  const width  = maxX + hexWidth + hexSpace
  const height = maxY + hexHeight + hexSpace
  html.setDim(dom,width,height)

  return dom
}
