
const uiPlayer = (p, s) => {
  const dom = div('player')
  dom.appendChild(uiBag("Bag",s._playerBag))
  dom.appendChild(uiBag("Available",s._playerAvailable))
  dom.appendChild(uiBag("Discarded",s._playerDiscarded))
  dom.appendChild(uiUpgrade(s._playerDevel))
  dom.appendChild(uiPlayerActions(s._playerBoard))
  return dom
}

const uiPlayerActions = (a) => {
  const dom = div('player-actions')
  for (g in a) {
    dom.appendChild(uiPlayerBoardGroup(a,a[g]))
  }
  return dom
}

const uiPlayerBoardGroup = (name,group) => {
  const dom = div('action-group')
  for (let i = 0; i < group.length; ++i) {
    const a = group[i]
    const hack = { tag: 'OneTime', contents: a.baBenefit }
    dom.appendChild(uiActivation(a._baCost,hack))
  }
  return dom
}

const uiBag = (name,bag) => {
  const dom = div('bag')
  setSize(dom,'height',iconSize)

  for (const r in bag) {
    const n = bag[r]
    if (n < 1) continue
    const c = uiCube(r)
    if (n > 0) addBadge(n,c)
    dom.appendChild(c)
  }
  tooltip(dom,true,name)

  return dom
}




const uiUpgrade = (bag) => {
  const dom = div('bag')
  setSize(dom,'height',iconSize)

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

    tooltip(r,false, 'Gain ' + color + ' cube at 4, 2 at 6')

    addBadge(n,r)
    dom.appendChild(r)
  }

  tooltip(dom,true,'Upgrades')
  return dom
}
