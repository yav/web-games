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
  const dom = div('question spot')
  setDim(dom,iconSize,iconSize)
  const req = spot.spotRequires
  const cl = req.tag === 'Exact' ? req.contents : 'wild'
  dom.classList.add(cl)
  setSize(dom,'margin',iconSize/8)
  const res = spot._spotResource
  if (res) dom.appendChild(uiCube(res))
  return dom
}

const uiActivation = (alt) => {
  const cost = alt._techCost
  const benefit = alt.techBenefit

  const dom = div('activation')

  const costDom = div('part')
  for (let i = 0; i < cost.length; ++i) {
    costDom.appendChild(uiSpot(cost[i]))
  }
  dom.appendChild(costDom)

  const bftPart = div('part')
  switch (benefit.tag) {
    case 'OneTime':
      bftPart.appendChild(uiAction(benefit.contents))
      break
    case 'Continuous': {
      const xxx = span(JSON.stringify(benefit.contents))
      bftPart.appendChild(xxx)
      break
    }
  }
  dom.appendChild(bftPart)
  return dom
}

const uiTech = (t) => {
  const dom = div('tech')
  setSize(dom,'width', 10 * iconSize)
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
  for (let i = 0; i < alts.length; ++i) {
    dom.appendChild(uiActivation(alts[i]))
  }
  return dom
}


