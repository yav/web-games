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


const uiActivation = (cost,benefit) => {
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
  const vp = div('part vp')
  vp.textContent = t.techVP
  h.appendChild(name)
  h.appendChild(vp)
  dom.appendChild(h)

  dom.appendChild(uiActivation(t.techCost,t.techBenefit))
  return dom
}


