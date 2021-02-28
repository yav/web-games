const hexSize   = 4 * iconSize
// const boardSize = 7 * hexSize
let originX = 0
let originY = 0

const uiHexLoc = (loc) => {
  const hexSpace = hexSize / 30
  return { x: originX + (loc.locX + loc.locY/2) * (hexSize + hexSpace)
         , y: originY + (-0.75 * loc.locY) * (hexSize + hexSpace)
         }
}


const uiSoldier = (el,pos,p,n,locked) => {
  const dom = div('icon')
  setDim(dom,iconSize,iconSize)
  setSize(dom,'left',pos.x)
  setSize(dom,'top',pos.y)
  el.appendChild(dom)

  const pic = svg('img/player.svg#helmet')
  pic.classList.add('player-' + playerColors[p])
  setDim(pic,iconSize,iconSize)
  dom.appendChild(pic)

  if (n != 1) {
    const b = badge(n)
    dom.appendChild(b)
  }

  // XXX: locked
  // XXX: interaction

  tooltip(dom,false, n + ' ' + p + ' troop' + (n > 1? 's' : ''))
}


const uiFort = (el,pos,p,num) => {
  // XXX
}


const uiCity = (el,pos,cityId, city) => {
  const dom = div('icon')
  setDim(dom,iconSize,iconSize)
  setSize(dom,'left',pos.x)
  setSize(dom,'top',pos.y)
  el.appendChild(dom)

  const capital = city.cityCapital
  if (capital) {
    const pic = svg('img/capitol.svg#capitol')
    pic.classList.add('player-' + playerColors[capital])
    setDim(pic,iconSize,iconSize)
    dom.appendChild(pic)
  }
  else dom.classList.add('city')

  const h = div('part')
  const help = uiAction(city.cityActions).dom
  h.appendChild(help)
  setSize(h,'left',pos.x)
  setSize(h,'top',pos.y + iconSize)
  let pinned = false
  dom.addEventListener('mouseenter',() => h.style.display = 'inline-block')
  dom.addEventListener('mouseleave',() => {
    if (!pinned) h.style.display = 'none'
  })
  dom.addEventListener('click',() => {
    h.style.display = 'inline-block'
    pinned = !pinned
  })
  h.style.display = 'none'
  el.appendChild(h)
}

const uiRuin = (el, pos, ruinId, ruin) => {
  const dom = div('icon')
  setDim(dom,iconSize,iconSize)
  setSize(dom,'left',pos.x)
  setSize(dom,'top',pos.y)
  el.appendChild(dom)
  dom.classList.add('ruins')
  setDim(dom,iconSize,iconSize)
  tooltip(dom,false,'Ruins')
}




const uiBoard = (b) => {
  const dom = div('board')
  let minX = 10 * iconSize
  let minY = 10 * iconSize
  let maxX = 0
  let maxY = 0
  for (let i = 0; i < b.length; ++i) {
    const pos = uiHexLoc(b[i][0])
    if (pos.x < minX) minX = pos.x
    if (pos.x > maxX) maxX = pos.x
    if (pos.y < minY) minY = pos.y
    if (pos.y > maxY) maxY = pos.y
  }
  const w = maxX - minX + hexSize
  const h = maxY - minY + hexSize
  setDim(dom,w,h)
  originX = -minX;
  originY = -minY;
  for (let i = 0; i < b.length; ++i) {
    uiHex(dom,b[i])
  }
  return dom
}


const uiHex = (container,info) => {

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
  container.appendChild(dom)


  const k  = hexSize / 4
  const ko = (k-iconSize) / 2
  const order = [ 2, 7, 5, 4, 1, 6, 3, 8, 9, 0 ]
  const start = 0 // Math.abs((loc.locX + loc.locY)) % 4

  let thing = 0
  const position = () => {
    const i = order[(start + thing) % order.length]
    let x = 0
    let y = 0
    if (i == 0 || i == 9) {
      x = 2 * k - iconSize /2
      y = ko
      if (i == 9) y += 3 * k
    } else {
      x = ko + ((i - 1) % 4) * k
      y = ko + (Math.floor ((i - 1) / 4) + 1) * k
    }
    return { x: pos.x + x, y: pos.y + y }
  }

  for (cityId in h.tileCities) {
    uiCity(container,position(),cityId, h.tileCities[cityId])
    ++thing
  }

  for (ruinId in h.tileRuins) {
    uiRuin(container,position(),ruinId, h.tileRuins[ruinId])
    ++thing
  }

  for (playerId in h.tilePlayers) {
    const player = h.tilePlayers[playerId]
    const fort = player.Fortification

    if (fort > 0) {
      uiFort(container,position(),playerId,fort)
      ++thing
    }

    const free = player.FreeUnit || 0
    const lock = player.LockedUnit || 0
    const tot  = free + lock
    console.log(free,lock)
    if (tot > 0) {
      uiSoldier(container,position(),playerId,tot, free === 0)
      ++thing
    }
  }

  return pos
}

