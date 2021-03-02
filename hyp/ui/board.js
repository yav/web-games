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


const uiSoldier = (el,pos,p,info) => {
  const dom = div('icon')
  setDim(dom,iconSize,iconSize)
  setSize(dom,'left',pos.x)
  setSize(dom,'top',pos.y)
  el.appendChild(dom)

  const pic = svg('img/player.svg#helmet')
  pic.classList.add('inner')
  pic.classList.add(playerColors[p])
  setDim(pic,iconSize,iconSize)
  dom.appendChild(pic)

  let fort = info.Fortification || 0
  let free = info.FreeUnit || 0
  let lock = info.LockedUnit || 0

  const counterDom = badge('')
  counterDom.classList.add('top')
  counterDom.classList.add('left')

  const fortDom = badge('')
  fortDom.classList.add('bottom')
  fortDom.classList.add('right')

  const lockDom = badge('')
  lockDom.innerHTML = '&#9876;'
  lockDom.classList.add('top')
  lockDom.classList.add('right')

  const update = () => {
    const tot = free + lock
    if (tot) {
      pic.style.display = 'inline-block'
      dom.style.backgroundColor = 'rgba(0,0,0,0.65)'
    } else {
      pic.style.display = 'none'
      dom.style.backgroundColor = 'rgba(0,0,0,0)'
    }
    counterDom.textContent = tot
    counterDom.style.display = (tot <= 1) ? 'none' : 'inline-block'
    fortDom.innerHTML =
      fort + '<span class="fg-' + playerColors[p] + '">&#9820;</span>'
    fortDom.style.display = (fort == 0) ? 'none' : 'inline-block'
    lockDom.style.display = (lock == 0 || free > 0) ? 'none' : 'inline-block'
  }

  update()
  dom.appendChild(counterDom)
  dom.appendChild(fortDom)
  dom.appendChild(lockDom)

  // XXX: methods
}


const uiOccupantOf = (dom) => {

  let occ

  return (it) => {
    if (occ) occ.remove()

    switch(it.tag) {
      case 'Empty': break
      case 'Ghost': {
        occ = badge('')
        occ.style.backgroundColor = 'black'
        setDim(occ,iconSize/2,iconSize/2)
        occ.classList.add('top')
        occ.classList.add('right')
        occ.classList.add('ghost')
        dom.appendChild(occ)
        break
      }
      case 'Occupied': {
        const player = it.contents
        occ = badge('')
        setDim(occ,iconSize/2,iconSize/2)
        occ.classList.add('top')
        occ.classList.add('right')
        occ.style.backgroundColor = 'black'

        const pic = svg('img/player.svg#helmet')
        pic.classList.add('inner')
        pic.classList.add(playerColors[player])
        setDim(pic,iconSize/2,iconSize/2)
        occ.appendChild(pic)

        dom.appendChild(occ)
        break
      }
    }
  }

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
    pic.classList.add('inner')
    pic.classList.add(playerColors[capital])
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

  const setOcc = uiOccupantOf(dom)
  setOcc(ruin.ruinSpot)

  let tokens = ruin.ruinTokens
  // XXX: tokens


  return {
    setOccupant: setOcc
  }

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

const uiHexAllocator = () => {
  const allocated = {}

  return {
    newLoc: () => {
      let i = 0
      while(allocated[i]) ++i
      allocated[i] = true
      return i
    },
    freeLoc: (i) => {
      delete allocated[i]
    }
  }
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
  const order = [ 2, 7, 5, 4, 9, 0, 1, 6, 3, 8 ]
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
    uiSoldier(container,position(),playerId,h.tilePlayers[playerId])
  }

  return pos
}

