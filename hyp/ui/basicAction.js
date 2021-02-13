
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
  if (!(n >= 0)) dom.classList.add('negative')
  return dom
}



const addBadge = (n,el) => {
  if (n == 1) return
  const b = badge(n)
  b.classList.add('left')
  el.appendChild(b)
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
    case 'AnyNormal': return 'non-gray'
  }
}

const cube = (color) => {
  let cl = ''
  switch (color.tag) {
    case 'Exact': cl = r.contents; break
    case 'AnyNormal': cl = 'wild'; break
  }
  const dom = div('icon')
  const size = 0.75 * iconSize
  setDim(dom,size,size)
  setSize(dom,'margin', (iconSize - size) /2)
  dom.classList.add(cl)
  return dom
}

const gainResource = (r) => {
  const dom = actionIcon('',resourceName(r))
  dom.appendChild(cube(r))
  return dom
}


const f = function(t) {
  let res
  hsBasicAction(

    { Times: (act,n) => {
        res = f(act)
        addBadge(n,res)
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
    , Spy:() => res = actionIcon('spy','Copy ability')
    , GainTech:() => res = actionIcon('gain-tech','Gain Technology')

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

