const uiSpot = (spot) => {
  const dom = div('spot')
  // XXX: cube
  setDim(dom,iconSize,iconSize)
  const req = spot.spotRequires
  const cl = req.tag === 'Exact' ? req.contents : req.tag
  dom.classList.add(cl)
  setSize(dom,'margin',iconSize/8)
  return dom
}


const uiTech = (t) => {
  console.log(t)
  const dom = div('tech')
  setSize(dom,'width', 10 * iconSize)
  setSize(dom,'margin', iconSize / 8)

  const h = div('heading')
  const name = div('part name')
  name.textContent = t.techName
  const vp = div('part vp')
  vp.textContent = t.techVP
  h.appendChild(name)
  h.appendChild(vp)
  dom.appendChild(h)

  const a = div('activation')
  dom.appendChild(a)

  const cost = t.techCost
  const costDom = div('part')
  for (let i = 0; i < cost.length; ++i) {
    costDom.appendChild(uiSpot(cost[i]))
  }
  a.appendChild(costDom)

  const bft = t.techBenefit
  const bftPart = div('part')
  switch (bft.tag) {
    case 'OneTime':
      bftPart.appendChild(uiAction(bft.contents))
      break
    case 'Continuous': {
      const xxx = span(JSON.stringify(bft.contents))
      bftPart.appendChild(xxx)
      break
    }
  }
  a.appendChild(bftPart)
  return dom
}
