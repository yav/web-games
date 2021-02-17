
const uiPlayer = (p, s) => {
  const dom = div('player')
  const ui = { bag:       uiBag("Bag",s._playerBag)
             , available: uiBag("Available",s._playerAvailable)
             , discarded: uiBag("Discarded",s._playerDiscarded)
             , updgrade:  uiUpgrade(s._playerDevel)
             , techs:     uiPlayerTech(s._playerTech)
             , dom: dom
             }
  dom.appendChild(ui.bag.dom)
  dom.appendChild(ui.available.dom)
  dom.appendChild(ui.discarded.dom)
  dom.appendChild(ui.updgrade.dom)
  dom.appendChild(ui.techs.dom)
  return ui
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
    if (n > 0) {
      const c = uiCube(r)
      addBadge(n,c)
      dom.appendChild(c)
      cubes[r] = { dom: c, number: n }
    } else cubes[r] = undefined
  }

  for (const r in bag) {
    const n = bag[r]
    if (n < 1) continue
    setR(r,n)
  }
  tooltip(dom,true,name)

  return {
      dom: dom
    , remove: (r) => {
        let info = cubes[r]
        if (info) info.dom.remove()
        setR(info? (info.number - 1) : 0, r)
      }
    , add: (r) => {
        let info = cubes[r]
        if (info) info.dom.remove()
        setR(info? (info.number + 1) : 1, r)
      }
  }
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

    addBadge(n,r)
    dom.appendChild(r)
  }

  const help = uiHelp( 'Upgrades'
                     , [ 'Reduce to 0 to add a cube of the corresponding' +
                         ' color to the bag.'
                       , 'At level 4/5 add 1 cube'
                       , 'At level 6 add 2 cubes.'])
  tooltipEl(dom,false,help)
  return { dom: dom }
}
