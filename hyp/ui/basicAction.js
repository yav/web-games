
const iconSize = 32



const tooltipEl = (el,above,n) => {
  const dom  = div('tooltip')
  const size = 0.4 * iconSize
  setSize(dom,'fontSize',0.8 * size)
  dom.appendChild(n)
  el.addEventListener('mouseenter',() => dom.style.display = 'inline-block')
  el.addEventListener('mouseleave',() => dom.style.display = 'none')
  el.appendChild(dom)
  setSize(dom,'left',0)
  setSize(dom,'top', above? -size * 0.9 : 1.1 * iconSize)
}

const tooltip = (el,above,n) => tooltipEl(el,above,span(n))


const uiAction = (act) => {
  const dom = div('action')
  const cont = act.contents
  switch (act.tag) {
    case 'If': {
      const a = div('action')
      uiBasicAction(a,cont[0])
      dom.appendChild(a)
      dom.appendChild(span(' to '))
      const as = cont[1]
      for (let i = 0; i < as.length; ++i) {
        uiBasicAction(dom,as[i])
      }
      return dom
    }
    case 'Action': {
      for (let i = 0; i < cont.length; ++i) {
        uiBasicAction(dom,cont[i])
      }
      return dom
    }
    case 'Or': {
      dom.appendChild(uiAction(cont[0]))
      dom.appendChild(span(' or '))
      dom.appendChild(uiAction(cont[1]))
      return dom
    }
  }

}

const badge = (n) => {
  const dom       = div('badge')
  const size      = 0.4 * iconSize
  setDim(dom,size,size)
  setSize(dom,'fontSize',0.8 * size)
  dom.textContent = n
  return dom
}




const uiBasicAction = function() {


const actionIcon = (n,action,help) => {
  const dom     = div('icon ' + action)
  const style   = dom.style
  setDim(dom,iconSize,iconSize)
  setSize(dom,'margin',iconSize/8)
  if (n != 1) {
    const b = badge(n)
    if (n < 0) b.classList.add('negative')
    dom.appendChild(b)
  }
  tooltip(dom,false,help)
  return dom
}

const neighbours = () => {
  const dom = div('icon Neighbours')
  const txt =
    'repeating-linear-gradient(0deg,#ccc,#ccc ' + (iconSize/4) + 'px, ' +
     '#999,#999 ' + (iconSize/4) + 'px, #999 ' + (iconSize/2) + 'px)'
  dom.style.backgroundImage = txt
  tooltip(dom,true,'Neighbors')
  return dom
}

const resourceName = (r) => {
  switch (r.tag) {
    case 'Exact': return r.contents
    case 'AnyNormal': return 'non-gray'
  }
}

const cube = (color,withCube) => {
  let cl = ''
  switch (color.tag) {
    case 'Exact': cl = r.contents; break
    case 'AnyNormal': cl = 'AnyNormal'; break
  }

  const dom    = div('pic ' + cl)
  const size   = iconSize * 0.75
  setDim(dom,size,size)
  if (withCube) (dom.appendChild(img('img/cube.svg')))
  return dom
}

const gainResource = (n,r) => {
  const help = n > 0 ? 'Gain ' : 'Loose '
  const dom = actionIcon(n,'',help + resourceName(r))
  dom.appendChild(cube(r,false))
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
    , RemoveWorker: (n) => c.appendChild(actionIcon(-n,'Deploy','Recall'))
    , PlaceWorker: (n) => c.appendChild(actionIcon(n,'Deploy','Deploy'))
    , CloneWorker: (n) => c.appendChild(actionIcon(n,'clone','Reinforcments'))
    , GainResource:(r,n) => {
        const it = actionIcon(n,'','Gain resource')
        const cl = r.tag === 'Exact' ? r.contents : r.tag
        it.appendChild(cube(cl))
        c.appendChild(it)
      }
    , DrawResource:(n) => c.appendChild(actionIcon(n,'draw','Draw Resource'))
    , Spy:(n) => c.appendChild(actionIcon(n,'spy','Copy ability'))
    , GainTech:(n) => c.appendChild(actionIcon(n,'gain-tech','Gain Technology'))
    , Develop: (how,n) => {
      let cl
      let help
      switch (how.tag) {
         case 'Same': cl = 'upgrade-eq'; help = 'Upgrade one'; break
         case 'Different':
           cl = 'upgrade-diff'; help = 'Upgrade different'; break
         default: cl = 'upgrade'; help = 'Upgrade'
      }
      c.appendChild(actionIcon(n,cl,help))
      }
    , LooseResource: (r,n) => c.appendChild(gainResource(-n,r))
    })(t)
  }
  return f
}()

