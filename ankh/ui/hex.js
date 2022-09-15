const hexWidthToHeight = Math.sin(Math.PI/3)
const hexSpace         = 2
const hexWidth         = 100
const hexHeight        = hexWidthToHeight * hexWidth
const wallWidth        = 0.55 * hexWidth
const wallHeight       = 0.1 * hexHeight


function hexLoc(gx,gy) {
  return { x: hexSpace + (hexWidth + hexSpace) * 0.75 * gx
         , y: hexSpace + (hexHeight + hexWidthToHeight * hexSpace) * (gy + gx / 2)
         }
}

function hex(gx,gy,sty) {
  const dom = html.div('hex ' + sty)
  html.setDim(dom,hexWidth,hexHeight)
  const loc = hexLoc(gx,gy)
  html.setLoc(dom,loc.x,loc.y)

  const border = html.div('border')
  const size   = 1.5 * hexSpace // a little bigger to avoid seams
  const hi     = size * hexWidthToHeight
  html.setDim(border, hexWidth + size, hexHeight + hi)
  html.setLoc(border, -size/2, -hi /2)
  dom.appendChild(border)

  const bg = html.div('bg')
  bg.textContent = gx + ',' + gy
  dom.appendChild(bg)
  return dom
}

function hexWall(gx,gy,d) {
  const dom = html.div('hex-wall')
  html.setDim(dom,wallWidth,wallHeight)

  const loc    = hexLoc(gx,gy)
  const locN   = hexLoc(gx,gy-1)
  const dy     = (locN.y - loc.y) / 2

  const cx     = loc.x + (hexWidth - wallWidth) / 2
  const cy     = loc.y + (hexHeight - wallHeight) / 2

  html.setLoc(dom, cx, cy)
  const s = dom.style
  s.transformOrigin = "center"
  s.transform = "rotate(" + (d * 60) + "deg) translate(0," + dy + "px)"

  return dom
}


