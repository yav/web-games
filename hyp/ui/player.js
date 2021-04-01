const uiPlayers = (turnOrder, players) => {
  let drawNext = 0
  const playerNum = turnOrder.length
  for (; drawNext < playerNum; ++drawNext) {
    if (turnOrder[drawNext] == playerId) break
  }
  if (drawNext >= playerNum) drawNext = 0

  const dom = div('players')

  const tabs = div('tabs')
  dom.appendChild(tabs)

  const ps = {}
  const pTabs = {}

  let selected
  const doSelect = (p) => {
    if (selected === p) return

    if (selected !== undefined) {
      ps[selected].dom.classList.add('hidden')
      pTabs[selected].classList.remove('selected')
    }

    selected = p
    ps[selected].dom.classList.remove('hidden')
    pTabs[selected].classList.add('selected')
  }

  const first = drawNext
  for (let todo = playerNum; todo > 0; --todo) {
    const p = turnOrder[drawNext]
    ps[p] = uiPlayer(p, players[p])
    ps[p].dom.classList.add('hidden')
    dom.appendChild(ps[p].dom)

    // tab
    const ptab = div('player-tab')
    pTabs[p] = ptab
    ptab.appendChild(uiPlayerBadge(p))
    tabs.appendChild(ptab)
    ptab.addEventListener('click', () => doSelect(p))

    ++drawNext
    if (drawNext >= playerNum) drawNext = 0
  }
  doSelect(turnOrder[first])

  return {
    dom: dom,
    players: ps
  }
}

const uiPlayerBadge = (p) => {
  const dom = div('player-badge')
  dom.textContent = p
  dom.classList.add(playerColors[p])
  dom.classList.add('text')
  return dom
}

const uiPlayer = (p, s) => {
  const dom = div('player')
  const bag = {}

  setSize(dom, 'width', 17.5 * iconSize)

  const label = { BagReady: 'Available', BagSource: 'In bag'
                , BagDiscard: 'Discarded' }
  for (b in s._playerBag) {
    bag[b] = uiBag(label[b],s._playerBag[b])
  }
  const ui = { bag: bag
             , stats:    uiPlayerStats(p,s)
             , upgrade:  uiUpgrade(s._playerDevel)
             , techs:    uiPlayerTech(s._playerTech)
             , dom: dom
             }
  dom.appendChild(ui.stats.dom)
  dom.appendChild(ui.bag['BagReady'].dom)
  dom.appendChild(ui.upgrade.dom)
  dom.appendChild(ui.bag['BagSource'].dom)
  dom.appendChild(ui.bag['BagDiscard'].dom)
  dom.appendChild(ui.techs.dom)
  return ui
}

const uiPlayerStats = (player,p) => {
  const dom = div('stats')

  let gems = p._playerGems
  const gemDom = actionIcon('gem','Gems')
  const gemBadge = addBadge(gems,gemDom,true)
  dom.appendChild(gemDom)

  let workers = p._playerWorkers
  const workerDom = actionIcon('deploy','Remaining troops')
  const workerBadge = addBadge(workers,workerDom,true)
  dom.appendChild(workerDom)

  let ghosts = p._playerGhosts
  const ghostDom = actionIcon('ghost','Captured ghosts')
  const ghostBadge = addBadge(ghosts,ghostDom,true)
  dom.appendChild(ghostDom)

  const addCaptured = (c) => {
    const cDom = div('icon')
    setDim(cDom,iconSize,iconSize)
    const pic = svg('img/player.svg#helmet')
    pic.classList.add('inner')
    pic.classList.add(playerColors[c])
    setDim(pic,iconSize,iconSize)
    cDom.appendChild(pic)
    tooltip(cDom,false,'Captured troop')
    dom.appendChild(cDom)
  }

  for (let i = 0; i < p._playerCaptured.length; ++i)
    addCaptured(p._playerCaptured[i])

  let token = null
  const setToken = (t) => {
    if (token) token.remove()
    if (!t) { token = null; return }

    if (typeof t === 'string') {
      token = div('icon token')
      token.classList.add(t)
      setSize(token,'font-size', iconSize/4)
      setDim(token,iconSize,iconSize)
      token.innerHTML = '<br>' + t
    } else {
      token = uiAction(t.tokenAction).dom
      token.classList.add('token')
    }
    dom.appendChild(token)
  }
  setToken(p._playerToken)



  const badge = uiPlayerBadge(player)
  badge.style.float = 'right'
  dom.appendChild(badge)

  return {
    dom: dom,
    changeGems: (n) => {
      gems += n
      gemBadge.textContent = gems
    },
    changeGhosts: (n) => {
      ghosts += n
      ghostBagde.textContent = ghosts
    },
    changeWorkers: (n) => {
      workers += n
      workerBadge.textContent = workers
    },
    capture: addCaptured,
    setToken: setToken
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
  return {
    dom: dom,
    tech: ts,
    ask: (tid,q) => existingQuestion(ts[tid].dom,true,q),
    add: (tid,t) => {
      ts[tid] = uiTech(t)
      dom.appendChild(ts[tid].dom)
    }
  }
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
    ask:    (r,q) => { existingQuestion(cubes[r].dom,false,q) }
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
    ask: (c,q) => { existingQuestion(number[c].dom,false,q) }
  }
}
