
const uiBasicAction = function() {

const iconSize = 32

const badge = function(n) {
  const dom       = div('badge')
  const size      = 0.4 * iconSize
  setDim(dom,size,size)
  setSize(dom,'fontSize',0.8 * size)
  dom.textContent = n
  return dom
}

const tooltip = function(el,above,n) {
  const dom  = div('tooltip')
  const size = 0.4 * iconSize
  setSize(dom,'fontSize',0.8 * size)
  dom.innerHTML = n
  el.addEventListener('mouseenter',() => dom.style.display = 'inline-block')
  el.addEventListener('mouseleave',() => dom.style.display = 'none')
  el.appendChild(dom)
  setSize(dom,'left',0)
  setSize(dom,'top', above? -size * 0.9 : 1.1 * iconSize)
}

const actionIcon = function(n,action,help) {
  const dom     = div('icon ' + action)
  const style   = dom.style
  setDim(dom,iconSize,iconSize)
  setSize(dom,'margin',2)
  if (n != 1) dom.appendChild(badge(n))
  tooltip(dom,false,help)
  return dom
}

const neighbours = function() {
  const dom = div('icon Neighbours')
  tooltip(dom,true,'Neighbors')
  return dom
}

const cube = function(color) {
  const dom    = div('pic ' + color)
  const size   = iconSize * 0.75
  setDim(dom,size,size)
  dom.appendChild(img('img/cube.svg'))
  return dom
}

const f = function(c,t) {
  hsBasicAction(
    { Move: (n) => c.appendChild(actionIcon(n,'Move','Movement'))
    , Fly: (n) => c.appendChild(actionIcon(n,'Fly','Fly'))
    , Fortify: (n) => c.appendChild(actionIcon(n,'Fortify','Fortify'))
    , RangedAttack: (n) =>
        c.appendChild(actionIcon(n,'RangedAttack','Ranged Attack'))
    , Attack: (n) => c.appendChild(actionIcon(n,'Attack','Attack'))
    , Neighbours: (n) => {
        const c1 = neighbours()
        f(c1,n)
        c.appendChild(c1)
      }
    , Gem: (n) => c.appendChild(actionIcon(n,'Gem','Gem'))
    })(t)
  }
  return f
}()

