const hexSize = 4 * iconSize
let originX   = 0
let originY   = 0


const uiBoard = (b) => {
  const dom = div('board')
  originX = 0
  originY = 0
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
  setDim(dom,w+iconSize,h+iconSize)
  originX = -minX + iconSize/2;
  originY = -minY + iconSize/2;

  const board = {}
  for (let i = 0; i < b.length; ++i) {
    const info = b[i]
    const loc = info[0]
    let ys = board[loc.locX]
    if (ys === undefined) { ys = {}; board[loc.locX] = ys }
    ys[loc.locY] = uiHex(dom,info)
  }

  const getHex = (loc) => board[loc.locX][loc.locY]

  return {
    dom: dom,
    askCity: (loc,id,q) => getHex(loc).cities[id].ask(q),
    askRuin: (loc,id,q) => getHex(loc).ruins[id].ask(q),
    askUnit: (loc,pid,q) => getHex(loc).askUnit(pid,q),
    askMap: (loc,act,q) => getHex(loc).askMap(act,q),
    changeUnit: (loc,pid,ty,n) => getHex(loc).changeUnit(pid,ty,n),
    setUnitHighlight: (loc,pid,yes) => getHex(loc).setUnitHighlight(pid,yes),
    setCity: (loc,id,x) => getHex(loc).cities[id].set(x),
    setRuin: (loc,id,x) => getHex(loc).ruins[id].set(x),
    dropToken: (loc,id) => getHex(loc).ruins[id].changeCount(-1),
    changeTile: (loc,t) => {
      getHex(loc).remove()
      board[loc.locX][loc.locY] = uiHex(dom,[loc,t])
    },
    hi:     (loc)    => getHex(loc).hi(),
    lo:     (loc)    => getHex(loc).lo(),
    hiCity: (loc,id) => getHex(loc).cities[id].hi(),
    loCity: (loc,id) => getHex(loc).cities[id].lo(),
    hiRuin: (loc,id) => getHex(loc).ruins[id].hi(),
    loRuin: (loc,id) => getHex(loc).ruins[id].lo()
  }
}


const uiHexLoc = (loc) => {
  const hexSpace = hexSize / 30
  return { x: originX + (loc.locX + loc.locY/2) * (hexSize + hexSpace)
         , y: originY + (-0.75 * loc.locY) * (hexSize + hexSpace)
         }
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
  dom.setAttribute('id', JSON.stringify(loc))
  setDim(dom,hexSize,hexSize)
  const pos = uiHexLoc(loc)
  setSize(dom,'left',pos.x)
  setSize(dom,'top',pos.y)

  const bord = div('hex')
  const w = 0.04
  setDim(bord,(1+2*w)*hexSize,(1+2*w)*hexSize)
  const off = w * hexSize
  setSize(bord,'left',pos.x - off)
  setSize(bord,'top',pos.y - off)
  bord.classList.add('border')
  container.appendChild(bord)
  if (h.tileCapital !== null) {
    bord.classList.add(playerColors[h.tileCapital])
    bord.style.zIndex = 1
  }

  const bg = div('bg')
  bg.classList.add(h.tileTerrain)
  dom.appendChild(bg)
  container.appendChild(dom)


  const k  = hexSize / 4
  const ko = (k-iconSize) / 2
  const order = [ 2, 7, 5, 4, 9, 0, 1, 6, 3, 8 ]
  const start = 0 // Math.abs((loc.locX + loc.locY)) % 4

  const alloc = uiHexAllocator()

  const position = (thing) => {
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

  const cities  = {}
  const ruins   = {}
  const units   = {}

  for (const cityId in h.tileCities) {
    const thing = alloc.newLoc()
    cities[cityId] =
      uiCity(container,position(thing),cityId, h.tileCities[cityId])
  }

  for (const ruinId in h.tileRuins) {
    const thing = alloc.newLoc()
    ruins[ruinId] =
      uiRuin(container,position(thing),ruinId, h.tileRuins[ruinId])
  }

  for (const playerId in h.tilePlayers) {
    const thing = alloc.newLoc()
    units[playerId] = {
      slot: thing,
      obj: uiSoldier(container,position(thing),playerId,h.tilePlayers[playerId])
    }
  }

  return {
    cities: cities,
    ruins: ruins,

    hi: () => bord.classList.add('hi'),
    lo: () => bord.classList.remove('hi'),

    askUnit: (pid,q) => { console.log("Player: " + pid); console.log(units); units[pid].obj.ask(q) },

    // assumes unit exits
    setUnitHighlight: (pid,yes) => units[pid].obj.setHighlight(yes),

    changeUnit: (pid,ty,n) => {
      let known = units[pid]
      if (known === undefined) {
        const thing = alloc.newLoc()
        known = {
          slot: thing,
          obj: uiSoldier(container,position(thing),pid,{})
        }
        units[pid] = known
      }
      const keep = known.obj.change(ty,n)
      if (!keep) {
        alloc.freeLoc(known.slot)
        delete units[pid]
      }
    },

    askMap: (act,q) => {
      const el = uiBasicAction(act)
      const slot = alloc.newLoc()
      const loc = position(slot)
      setSize(el,'left',loc.x)
      setSize(el,'top',loc.y)
      container.appendChild(el)
      newQuestion(el,q,() => alloc.freeLoc(slot))
    },

    remove: () => dom.remove()
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

  const units = info._pUnits || {}
  let fort = units.Fortification || 0
  let free = units.FreeUnit || 0
  let lock = units.BlockedUnit || 0

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

  const setHighlight = (yes) => {
    if (yes) dom.classList.add('highlight')
        else dom.classList.remove('highlight')
  }

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
  setHighlight(info._pHighlight)
  dom.appendChild(counterDom)
  dom.appendChild(fortDom)
  dom.appendChild(lockDom)

  return {
    setHighlight: setHighlight,
    change: (ty,n) => {
      switch(ty) {
        case 'FreeUnit': free += n; break
        case 'BlockedUnit': lock += n; break
        case 'Fortification': fort += n; break
      }
      update()
      return free + lock + fort > 0
    },
    ask: (q) => existingQuestion(dom,false,q)
  }
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

  dom.classList.add('city')

  const h = div('part')
  const help = uiAction(city.cityActions).dom
  h.appendChild(help)
  setSize(h,'left',pos.x - iconSize * 0.5)
  setSize(h,'top',pos.y - iconSize * 1.5)
  let pinned = false
  dom.addEventListener('mouseenter',() => h.style.display = 'inline-block')
  dom.addEventListener('mouseleave',() => {
    if (!pinned) h.style.display = 'none'
  })
  h.style.display = 'none'
  el.appendChild(h)


  const setOcc = uiOccupantOf(dom)
  setOcc(city._citySpot)

  return {
    set: setOcc,
    ask: (q) => existingQuestion(dom,false,q),
    hi: () => dom.classList.add('hi'),
    lo: () => dom.classList.remove('hi')
  }
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
  const tokenDom = badge(tokens)
  tokenDom.classList.add('top')
  tokenDom.classList.add('left')
  tokenDom.classList.add(ruin.ruinType)
  dom.appendChild(tokenDom)


  return {
    set: setOcc,
    ask: (q) => existingQuestion(dom,false,q),
    hi: () => dom.classList.add('hi'),
    lo: () => dom.classList.remove('hi'),
    changeCount: (n) => {
      tokens = tokens + n
      tokenDom.textContent = tokens
    }

  }

}






