
const uiPlayer = (p, s) => {
  const dom = div('player')
  const bag = {}

  for (b in s._playerBag) {
    bag[b] = uiBag(b,s._playerBag[b])
  }
  const ui = { bag: bag
             , stats:    uiPlayerStats(s)
             , upgrade:  uiUpgrade(s._playerDevel)
             , techs:    uiPlayerTech(s._playerTech)
             , dom: dom
             }
  dom.appendChild(ui.stats.dom)
  dom.appendChild(ui.bag['BagSource'].dom)
  dom.appendChild(ui.bag['BagReady'].dom)
  dom.appendChild(ui.bag['BagDiscard'].dom)
  dom.appendChild(ui.upgrade.dom)
  dom.appendChild(ui.techs.dom)
  return ui
}

const uiPlayerStats = (p) => {
  const dom = div('stats')

  let gems = p._playerGems
  const gemDom = actionIcon('gem','Gems')
  const gemBadge = addBadge(gems,gemDom,true)
  dom.appendChild(gemDom)

  return {
    dom: dom,
    changeGems: (n) => {
      gems += n
      gemBadge.textContent = gems
    }
  }
}

const uiPlayerTech = (a) => {
  const dom = div('player-actions')
  const ts = []
  for (g in a) {
    ts[g] = uiTech(a[g])
  }
  for (let i = 0; i < ts.length; ++i) {
    dom.appendChild(ts[i].dom)
  }
  return { dom: dom, tech: ts }
}



const uiBag = (name,bag) => {
  const dom = div('bag')
  setSize(dom,'height',iconSize)

  const cubes = {}

  const setR = (r,n) => {
    let cur = cubes[r]
    if (n === 0) {
      if (cur) cur.dom.remove()
      cubes[r] = undefined
      return
    }

    if (!cur) {
      cur = { dom: uiCube(r), badge: null, number: n }
      dom.appendChild(cur.dom)
    }
    else cur.number = n

    if (n === 1) {
      if (cur.badge) { cur.badge.remove(); cur.badge = null }
    } else {
      if (!cur.badge) cur.badge = addBadge(n,cur.dom)
      else cur.badge.textContent = n
    }
    cubes[r] = cur
  }

  for (const r in bag) {
    const n = bag[r]
    if (n < 1) continue
    setR(r,n)
  }
  tooltip(dom,true,name)

  return {
    dom: dom,
    change: (r,n) => {
      const x = cubes[r] ? cubes[r].number : 0
      setR(r, x+n)
    },
    ask:    (r,q) => { existingQuestion(cubes[r].dom,q) }
  }
}




const uiUpgrade = (bag) => {
  const dom = div('bag')
  setSize(dom,'height',iconSize)

  const number = {}

  for (const color in bag) {
    const n = bag[color]
    const r = div('icon')
    const size = 0.75 * iconSize
    setDim(r,size,size)
    setSize(r,'margin', (iconSize - size) /2)

    const c = svg('img/upgrade.svg#upgrade')
    setDim(c,size,size)
    c.classList.add('inner')
    c.classList.add(color)
    r.appendChild(c)

    const ba = addBadge(n,r,true)
    number[color] = { dom: r, ba: ba, number: n }
    dom.appendChild(r)
  }

  const help = uiHelp( 'Upgrades'
                     , [ 'Reduce to 0 to add a cube of the corresponding' +
                         ' color to the bag.'
                       , 'At level 4/5 add 1 cube'
                       , 'At level 6 add 2 cubes.'])
  tooltipEl(dom,false,help)
  return {
    dom: dom,
    reset: (c) => {
      const ui = number[c]
      ui.number = 0
      ui.ba.textContent = ui.number
    },
    change: (c,n) => {
      const ui = number[c]
      ui.number = ui.number + n
      if (ui.number < 0) ui.number = 0
      ui.ba.textContent = ui.number
    },
    ask: (c,q) => { existingQuestion(number[c].dom,q) }
  }
}
