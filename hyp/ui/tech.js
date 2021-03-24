const innerIcon = () => {
  const dom = div('icon')
  const size = 0.75 * iconSize
  setDim(dom,size,size)
  setSize(dom,'margin', (iconSize - size) /2)
  return dom
}

const uiCube = (color) => {
  const r = div('icon')
  const size = 0.75 * iconSize
  setDim(r,size,size)
  setSize(r,'margin', (iconSize - size) /2)

  const c = svg('img/cube.svg#cube')
  setDim(c,size,size)
  c.classList.add('inner')
  c.classList.add(color)
  r.appendChild(c)

  tooltip(r,false,color + ' cube')
  return r
}

const uiSpot = (spot) => {
  const dom = div('spot')
  setDim(dom,iconSize,iconSize)
  const req = spot.spotRequires
  const cl = req.tag === 'Exact' ? req.contents : 'wild'
  dom.classList.add(cl)
  setSize(dom,'margin',iconSize/8)
  const res = spot._spotResource
  let resDom = null
  if (res) {
    resDom = uiCube(res)
    dom.appendChild(resDom)
  }
  return {
    dom: dom,
    add: (r) => {
      if (resDom) resDom.remove()
      resDom = uiCube(r)
      dom.appendChild(resDom)
    },
    remove: () => {
      if (resDom) resDom.remove()
    }
  }
}

const uiActivation = (alt) => {
  const cost = alt._techCost
  const benefit = alt.techBenefit

  const dom = div('activation')

  const costDom = div('part')
  const spot = []
  for (let i = 0; i < cost.length; ++i) {
    spot[i] = uiSpot(cost[i])
    costDom.appendChild(spot[i].dom)
  }
  dom.appendChild(costDom)

  const bftPart = div('part')
  switch (benefit.tag) {
    case 'OneTime':
      bftPart.appendChild(uiAction(benefit.contents).dom)
      break
    case 'Continuous': {
      bftPart.appendChild(uiContAction(benefit.contents))
      break
    }
  }
  dom.appendChild(bftPart)
  return { dom: dom, spot: spot }
}

const uiTech = (t) => {
  const dom = div('tech')
  setSize(dom,'width', 8.5 * iconSize)
  setSize(dom,'margin', iconSize / 8)

  const h = div('heading')
  const name = div('part name')
  name.textContent = t.techName
  h.appendChild(name)

  const vp = t.techVP
  if (vp > 0) {
    const vpDom = div('part vp')
    vpDom.textContent = vp
    h.appendChild(vpDom)
    tooltip(vpDom,true,'Endgame VP')
  }
  dom.appendChild(h)

  const alts = t._techAlts
  const as = []
  for (let i = 0; i < alts.length; ++i) {
    as[i] = uiActivation(alts[i])
    dom.appendChild(as[i].dom)
  }
  return {
    dom: dom,
    alt: as,
    ask: (q) => existingQuestion(dom,q)
  }
}


