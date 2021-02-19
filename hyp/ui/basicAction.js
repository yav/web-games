
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
  return dom
}

const tooltip = (el,above,n) => { return tooltipEl(el,above,span(n)) }

const uiHelp = (title,ps) => {
  const dom     = div('help')
  const h       = div('heading')
  h.textContent = title
  dom.appendChild(h)

  for (let i = 0; i < ps.length; ++i) {
    const p = document.createElement('p')
    p.textContent = ps[i]
    dom.appendChild(p)
  }

  return dom
}


const uiAction = (act) => {
  const dom = div('action')
  const cont = act.contents
  switch (act.tag) {
    case 'If': {
      const a = div('action')
      a.appendChild(uiBasicAction(cont[0]))
      dom.appendChild(a)
      dom.appendChild(span(' to '))
      const as = cont[1]
      for (let i = 0; i < as.length; ++i) {
        dom.appendChild(uiBasicAction(as[i]))
      }
      return dom
    }
    case 'Action': {
      for (let i = 0; i < cont.length; ++i) {
        dom.appendChild(uiBasicAction(cont[i]))
      }
      return dom
    }
    case 'Or': {
      dom.appendChild(uiBasicAction(cont[0]))
      dom.appendChild(span(' or '))
      dom.appendChild(uiBasicAction(cont[1]))
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
  if (!(n >= 0)) dom.classList.add('negative')
  return dom
}



const addBadge = (n,el,always) => {
  if (!always && n == 1) return null
  const b = badge(n)
  b.classList.add('left')
  el.appendChild(b)
  return b
}


const addBadgeRight = (n,el) => {
  const b = badge(n)
  b.classList.add('right')
  el.appendChild(b)
}




const uiBasicAction = function() {


const actionIcon = (action,help) => {
  const dom     = div('icon ' + action)
  const style   = dom.style
  setDim(dom,iconSize,iconSize)
  setSize(dom,'margin',iconSize/8)
  tooltip(dom,false,help)
  return dom
}

const neighbours = () => {
  const dom = div('icon neighbours')
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
    case 'AnyNormal': return 'wild'
  }
}

const cube = (color) => {
  const dom = div('icon')
  const size = 0.75 * iconSize
  setDim(dom,size,size)
  setSize(dom,'margin', (iconSize - size) /2)
  dom.classList.add(resourceName(color))
  return dom
}

const gainResource = (r) => {
  const dom = actionIcon('',resourceName(r))
  dom.appendChild(cube(r))
  return dom
}

const swapResource = (a,b) => {
  const dom = div('icon')
  setSize(dom,'width',2 * iconSize)
  setSize(dom,'margin',iconSize/8)
  const x = cube(a)
  const y = cube(b)
  const z = svg('img/upgrade.svg#upgrade')
  setDim(z,0.5 * iconSize, 0.5 * iconSize)
  z.style.fill = '#fff'
  z.style.transform = 'rotate(90deg)'
  setSize(z,'left', 0.75 * iconSize)
  setSize(z,'top',  0.25 * iconSize)
  dom.appendChild(x)
  dom.appendChild(y)
  dom.appendChild(z)
  tooltip(dom,false,'Change a resource')
  return dom
}


const f = function(t) {
  let res
  hsBasicAction(

    { Times: (act,n) => {
        res = f(act)
        if (n !== 1) addBadge(n,res)
      }

    , Neighbours: (act) => {
        const tmp = f(act)
        res = neighbours()
        res.appendChild(tmp)
      }


    , Move:    () => res = actionIcon('move','Movement')
    , Fly:     () => res = actionIcon('fly','Fly')
    , Fortify: () => res = actionIcon('fortify','Fortify')
    , RangedAttack: () => res = actionIcon('rangedAttack','Ranged Attack')
    , Attack: () => res = actionIcon('attack','Attack')
    , Gem: () => res = actionIcon('gem','Gem')
    , PlaceWorker: () => res = actionIcon('deploy','Deploy')
    , CloneWorker: () => res = actionIcon('clone','Reinforcments')
    , GainResource:(r) => {
        res = actionIcon('','Gain resource')
        res.appendChild(cube(r))
      }
    , DrawResource:() => res = actionIcon('draw','Draw Resource')
    , ReturnResource:() => res = actionIcon('return','Return to bag')
    , Spy:() => res = actionIcon('spy','Copy ability')
    , GainTech:() => res = actionIcon('gain-tech','Gain Technology')

    , SwapResource: (a,b) => res = swapResource(a,b)
    , LooseGem:() => {
        res = actionIcon('gem','Loose gem')
        addBadgeRight('x',res)
      }
    , RemoveWorker: (n) => {
        res = actionIcon('deploy', 'Recall')
        addBadgeRight('x',res)
      }

    , LooseResource: (r) => {
        res = actionIcon('','Loose resource')
        res.appendChild(cube(r))
        addBadgeRight('x',res)
      }

    , LooseDevelop: () => {
        res = actionIcon('upgrade', 'Loose 1 upgrade step')
        addBadgeRight('x',res)
      }

    , Develop: (how) => {
      switch (how.tag) {
         case 'Same': {
            const n = how.contents
            const help = 'Upgrade one ' + n + ' steps';
            res = actionIcon('upgrade-eq',help)
            addBadgeRight(n,res)
            break
         }
         case 'Different': {
           const n = how.contents
           help = 'Upgrade ' + n + ' different';
           res = actionIcon('upgrade-diff',help)
           addBadgeRight(n,res)
           break
         }
         default: {
           res = actionIcon('upgrade', 'Upgrade')
         }
      }
      }
    })(t)
    if (!res) {
      console.log("UNDEFINED RESULT:")
      console.log(t)
    }
    return res
  }
  return f
}()

