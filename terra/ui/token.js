const genericToken = (cs,img) => {
  const dom = html.svg('img/' + img + '.svg#it')
  html.setDim(dom,iconSize,iconSize)
  for (let i = 0; i < cs.length; ++i) dom.classList.add(cs[i])
  dom.classList.add('token')
  return dom
}


const playerToken   = (player,img) =>
                          genericToken(['square',playerColors[player]],img)
const explorerToken = (player) => playerToken(player,'meeple')
const chieftanToken = (player) => playerToken(player,'meeple-king')
const swordToken    = (player) => playerToken(player,'swords-emblem')
const boatToken     = (player) => playerToken(player,'wood-canoe')
const cartToken     = (player) => playerToken(player,'old-wagon')
const flagToken     = (player) => playerToken(player,'flying-flag')

const bisonToken    = () => genericToken(['round','animal'],'bison')
const leatherToken  = () => genericToken(['square','animal'],'animal-hide')
const woodToken     = () => genericToken(['round','wood'],'log')
const lumberToken   = () => genericToken(['square','wood'],'wood-beam')
const stoneToken    = () => genericToken(['round','stone'],'stone-pile')
const blockToken    = () => genericToken(['square','stone'],'stone-block')
const oreToken      = () => genericToken(['round','ore'],'ore')
const metalToken    = () => genericToken(['square','ore'],'metal-bar')

const tokenStack    = (els) => {
  const dom = html.div('stack')
  let dx = 0
  let delta = iconSize / 5

  html.setDim(dom,iconSize,iconSize)
  for (let i = 0; i < els.length; ++i) {
    const el = els[i]
    const w = iconSize / els.length
    html.setDim(el,w,w)
    el.style.position = 'absolute'
    html.setSize(el, 'left', dx)
    html.setSize(el, 'bottom', dx)
    dom.appendChild(el)
  }

  dom.addEventListener('mouseenter',() => {
    for (let i = 0; i < els.length; ++i) {
      els[i].style.position = 'relative'
      els[i].style.display = 'inline-block'
    }
  })

  dom.addEventListener('mouseleave',() => {
    for (let i = 0; i < els.length; ++i) {
      els[i].style.position = 'absolute'
    }
  })

 

  return dom
}


