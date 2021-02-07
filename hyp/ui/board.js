const hexSize   = 100
const boardSize = 7 * hexSize

const uiHexLoc = (loc) => {
  const hexSpace = hexSize / 30
  const origin   = boardSize / 2 - hexSize / 2
  return { x: origin + (loc.locX + loc.locY/2) * (hexSize + hexSpace)
         , y: origin + (-0.75 * loc.locY) * (hexSize + hexSpace)
         }
}

const uiSoldier = (n,p) => {
  const dom = div('icon')
  setDim(dom,iconSize,iconSize)

  const pic = svg('img/crested-helmet.svg#helmet')
  pic.classList.add('player-' + p)
  setDim(pic,iconSize,iconSize)
  dom.appendChild(pic)

  if (n != 1) {
    const b = badge(n)
    if (n < 0) b.classList.add('negative')
    dom.appendChild(b)
  }

  tooltip(dom,false, n + ' ' + p + ' troop' + (n > 1? 's' : ''))
  return dom
}



const uiBoard = (b) => {
  const dom = div('board')
  for (let i = 0; i < b.length; ++i)
    dom.appendChild(uiHex(b[i]))
  setDim(dom,boardSize,boardSize)
  return dom
}

const uiHex = (info) => {
  const loc = info[0]
  const h   = info[1]

  const dom = div('hex')
  setDim(dom,hexSize,hexSize)
  const pos = uiHexLoc(loc)
  setSize(dom,'left',pos.x)
  setSize(dom,'top',pos.y)

  const bg = div('bg')
  bg.classList.add(h.tileTerrain)
  dom.appendChild(bg)

  return dom
}

