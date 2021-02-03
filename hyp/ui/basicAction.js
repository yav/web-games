const iconSize = 32

function badge(n) {
  const dom       = div('badge')
  const size      = 0.4 * iconSize
  setDim(dom,size,size)
  setSize(dom,'fontSize',0.8 * size)
  dom.textContent = n
  return dom
}

function tooltip(el,n) {
  const dom  = div('tooltip')
  const size = 0.4 * iconSize
  setDim(dom,size,size)
  setSize(dom,'fontSize',0.8 * size)
  dom.textContent = n
  return dom
}

function actionIcon(n,action,help) {
  const dom     = div('icon ' + action)
  const style   = dom.style
  setDim(dom,iconSize,iconSize)
  if (n != 1) dom.appendChild(badge(n))
  tooltip(dom,help)
  return dom
}

function cube(color) {
  const dom    = div('pic ' + color)
  const size   = iconSize * 0.75
  setDim(dom,size,size)
  dom.appendChild(img('img/cube.svg'))
  return dom
}

const uiBasicAction = hsBasicAction(
  { Move: (n) => gui.container.appendChild(actionIcon(n,'move','Movement'))
  })


