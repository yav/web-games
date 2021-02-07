const hexSize   = 5 * iconSize
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

const uiCity = (cityId, city) => {
  const dom = div('icon')
  dom.classList.add(city.cityCapital ? 'capitol' : 'city')
  setDim(dom,iconSize,iconSize)
  const h = div('part')
  const help = uiAction(city.cityActions)
  h.appendChild(help)
  setSize(h,'left',0)
  setSize(h,'top',iconSize)
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
  dom.appendChild(h)
  return dom
}

const uiRuin = (ruinId, ruin) => {
  const dom = div('icon')
  dom.classList.add('ruins')
  setDim(dom,iconSize,iconSize)
  tooltip(dom,false,'Ruins')
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


  const k  = hexSize / 4
  const ko = (k-iconSize) / 2
  const order = [ 2, 7, 5, 4, 1, 6, 3, 8, 9, 0 ]
  const start = Math.abs((loc.locX + loc.locY)) % 4

  let thing = 0
  const position = (el) => {
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
    setSize(el,'left', x)
    setSize(el,'top',  y)
  }

  for (cityId in h.tileCities) {
    const el = uiCity(cityId, h.tileCities[cityId])
    position(el)
    dom.appendChild(el)
    ++thing
  }

  for (ruinId in h.tileRuins) {
    const el = uiRuin(ruinId, h.tileRuins[ruinId])
    position(el)
    dom.appendChild(el)
    ++thing
  }



  return dom
}

