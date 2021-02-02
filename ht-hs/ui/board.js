function boardCoord(name,size) {
  const map = layout[name]
  const wi = map.width
  const hi = map.height
  const larger = Math.max(wi,hi)
  const sc = size / larger
  const sz = sc * map.worker
  const cosc = sc * hi / 1000 // coordinates are relative to height 1000

  return { width:      wi * sc
         , height:     hi * sc
         , url:        map.url

         , workerSize: sz
         , bonusSize:  3.9 * sz
         , houseSize:  1.5 * sz
         , upgradeSize: 4.5 * sz
         , fontSize: 2 * sz

         , tokenCountSpot: { x: cosc * map.bonus.x, y: cosc * map.bonus.y }

         , nodeSpot: function(node,ix) {
             const it = map.nodes[node][ix]
             if (it === undefined) return undefined
             return { x: cosc * it.x, y: cosc * it.y }
           }

         , nodeRadius: function(nodeId) {
             const node = map.nodes[nodeId]
             let xMin,yMin,xMax
             for (const spot in node) {
                const o = node[spot]
                const x = cosc * o.x
                const y = cosc * o.y
                if (xMin === undefined || x < xMin) xMin = x
                if (xMax === undefined || x > xMax) xMax = x
                if (yMin === undefined || y < yMin) yMin = y
             }
             return { x: xMin - 1.5*sz, y: yMin - sz
                    , width: 4*sz + xMax - xMin
                    , height: 4.5*sz }
           }

         , edgeSpot: function(edge,ix) {
             const it = map.edges[edge].spots[ix]
             return { x: cosc * it.x, y: cosc * it.y }
           }

         , edgeLen: function(edge) {
              return map.edges[edge].spots.length
           }

         , bonusSpot: function(edge) {
             const it = map.edges[edge]
             return { x: cosc * it.x, y: cosc * it.y, rotate: it.rotate }
           }

         , ptsSpot: function(ix) {
             const it = map.pts[ix - 1]
             return { x: cosc * it.x, y: cosc * it.y }
           }

         , fullSpot: function(ix,lim) {
            const fullLoc1 = map.house1
            const fullLoc2 = map.house2
            const fullDiff = (fullLoc2.x - fullLoc1.x) / lim
            return { x: cosc * (fullLoc1.x + ix * fullDiff)
                   , y: cosc * fullLoc1.y
                   }
            }

          , upgradeSpot: function(name) {
              const loc = map[name]
              return { x: cosc * loc.x, y: cosc * loc.y }
            }
         }
}


// for debug
function fillMap() {
  const map = layout[gui.board.mapName]
  const w = { owner: playerId, shape: 'Cube' }
  for (const i in map.nodes) {
    for (const j in map.nodes[i]) {
      gui.board.placeWorkerInOffice(i,w)
    }
  }

  for (const i in map.edges) {
  }
}


function drawBoard(opts) {
  const ui    = {}
  const board = boardCoord(opts.map, opts.size)
  ui.workerSize = board.workerSize
  ui.bonusSize  = board.bonusSize
  ui.fontSize   = board.fontSize
  ui.tokenCountSpot = board.tokenCountSpot
  ui.mapName = opts.map

  const dom = function() {
    const dom = document.createElement('div')
    dom.classList.add('board')
    dom.style.height = board.height
    dom.style.width  = board.width
    dom.style.fontSize = board.fontSize

    const img = document.createElement('img')
    img.classList.add('board-img')
    img.setAttribute('src', board.url)
    dom.appendChild(img)

    return dom
  } ()

  { // Full marker
    const el = document.createElement('div')
    el.classList.add('fullMarker')
    const style = el.style
    style.width  = board.houseSize
    style.height = board.houseSize
    const setFull = function (spot) {
      const loc = board.fullSpot(spot,opts.fullMax)
      style.left = loc.x
      style.top  = loc.y
    }
    setFull(opts.full)
    dom.appendChild(el)

    // exported:
    ui.setFull = setFull
  }


  { // offices
    const lastAnnex = {} // locaiton of last annex, indexed by node

    const nextAnnexLoc = function(node,shape) {
      let loc = lastAnnex[node]
      if (loc === undefined) {
        const tmp = board.nodeSpot(node,0)
        loc = { x: tmp.x - 0.5 * board.workerSize, y: tmp.y }
      }
      return { x: loc.x - 1.5 * workerSize(board.workerSize,shape), y: loc.y }
    }

    const addAnnex = function(node,worker) {
      const loc = nextAnnexLoc(node,worker.shape)
      const el  = drawWorkerAt(loc,board.workerSize,worker)
      lastAnnex[node] = loc
      dom.appendChild(el)
    }

    const askAnnex = function(node,shape,q) {
      const loc = nextAnnexLoc(node,shape)
      const worker = { owner: playerId, shape: shape }
      const el  = drawWorkerAt(loc,board.workerSize,worker)
      el.classList.add('empty')
      dom.appendChild(el)
      gui.questionNew(el,q)
    }

    const placedOffices = {} // info about offices in a node

    const nextOfficeLoc = function(node) {
      let placed = placedOffices[node]
      if (placed === undefined) placed = []
      const spot = placed.length

      const loc = board.nodeSpot(node,spot)
      if (loc === undefined) {
        const last = placed[spot - 1]
        return { x: last.loc.x + 1.5 * workerSize(board.workerSize,last.shape)
              , y: last.loc.y
              }
      }
      return loc
    }


    const askFullOffice = function(node,spot,q) {
      const info = placedOffices[node][spot]
      gui.questionAnnot(info.dom,q)
    }

    const askEmptyOffice = function(node,shape,q) {
      const loc = nextOfficeLoc(node)
      const worker = { owner: playerId, shape: shape }
      const el = drawWorkerAt(loc,board.workerSize,worker)
      el.classList.add('empty')
      dom.appendChild(el)
      gui.questionNew(el,q)
    }

    const addOffice = function(node,worker) {
      const loc = nextOfficeLoc(node)
      const el  = drawWorkerAt(loc,board.workerSize,worker)
      let placed = placedOffices[node]
      if (placed === undefined) { placed = []; placedOffices[node] = placed }
      placed[placed.length] = { loc: loc, worker: worker, dom: el }
      dom.appendChild(el)
    }

    const swapWorkers = function(node,spot) {
      const info = placedOffices[node]
      const w1   = info[spot]
      const w2   = info[spot-1]
      w1.dom.style.left = w2.loc.x
      w1.dom.style.top  = w2.loc.y
      w2.dom.style.left = w1.loc.x
      w2.dom.style.top  = w1.loc.y
      const tmp = w2.loc
      w2.loc = w1.loc
      w1.loc = tmp
      info[spot] = w2
      info[spot-1] = w1
    }


    // Initialize
    const nodeNames = {}

    for (const i in opts.nodes) {
      const node = opts.nodes[i]
      nodeNames[i] = node.name
      for (let j = 0; j < node.annex.length; ++j) addAnnex(i,node.annex[j])
      for (let j = 0; j < node.office.length; ++j) addOffice(i,node.office[j])
    }

    // exported
    ui.nodeNames           = nodeNames
    ui.placeWorkerInAnnex  = addAnnex
    ui.askAnnex            = askAnnex
    ui.placeWorkerInOffice = addOffice
    ui.askFullOffice       = askFullOffice
    ui.askEmptyOffice      = askEmptyOffice
    ui.swapWorkers         = swapWorkers

  }

  { // help for actions
    const stats =
      { Actions:   'Actions'
      , Movement:  'Movement'
      , Hiring:    'Hiring'
      , Privilege: 'Privilege'
      , Keys:      'Keys'
      }

    const makeBox = function(loc,n,lab) {
      const el = document.createElement('div')
      el.classList.add('upgrade-action')
      el.classList.add('help')

      const help = document.createElement('div')
      el.appendChild(help)
      help.classList.add('label')
      help.textContent = lab

      const style  = el.style
      style.width  = n * board.upgradeSize
      style.height = board.upgradeSize
      style.left   = loc.x
      style.top    = loc.y
      dom.appendChild(el)
    }

    for (stat in stats) {
      makeBox(board.upgradeSpot(stat), 1, stats[stat])
    }
    const loc = board.ptsSpot(1)
    makeBox(loc, 3, "End game VP")

  }

  { // nodes
    const nodeBox = {}
    ui.hilightNode = function(node) {
      let val = nodeBox[node]
      if (val === undefined) {
        const y = document.createElement('div')
        dom.appendChild(y)
        y.classList.add('node-highlight')
        const loc = board.nodeRadius(node)
        y.style.position = 'absolute'
        y.style.left   = loc.x
        y.style.top    = loc.y
        y.style.width  = loc.width
        y.style.height = loc.height
        nodeBox[node] = y
        val = y
      }
      val.style.display = 'inline-block'
    }

    ui.unhilightNode = function(node) {
      const it = nodeBox[node]
      if (it === undefined) return
      it.style.display = 'none'
    }
  }


  { // edges

    const placedWorkers = {}

    const makeWorker = function(edge,spot,worker) {
      const loc = board.edgeSpot(edge,spot)
      const b = drawWorkerAt(loc,board.workerSize,worker)
      dom.appendChild(b)
      return b
    }

    const placeWorker = function(edge,spot,worker) {
      const el = makeWorker(edge,spot,worker)
      let placed = placedWorkers[edge]
      if (placed === undefined) { placed = {}; placedWorkers[edge] = placed }
      placed[spot] = el
    }

    const askEmptyWorker = function(edge,spot,shape,json) {
      const w = { owner: playerId, shape: shape }
      const b = makeWorker(edge,spot,w)
      b.classList.add('empty')
      gui.questionNew(b, json)
    }

    const askFullWorker = function(edge,spot,mbShape,worker,json) {
      const w = placedWorkers[edge][spot]
      gui.questionAnnot(w,json)
    }

    const removeWorker = function(edge,spot) {
      const xs = placedWorkers[edge]
      const el = xs[spot]
      delete xs[spot]
      el.remove()
    }

    const placedBonuses = {}

    const placeBonus = function(edge,bonus) {
      const loc = board.bonusSpot(edge)
      const el  = drawBonusTokenAt(loc, board.bonusSize, bonus)
      dom.appendChild(el)
      placedBonuses[edge] = el
    }

    const askEdge = function(edge,q) {
      const n    = board.edgeLen(edge) - 1
      for (let i = 0; i < n; ++i) {
        const from = board.edgeSpot(edge,i)
        const to   = board.edgeSpot(edge,i+1)
        const it = document.createElement('div')
        it.classList.add('edge-marker')
        const style = it.style
        const w = board.workerSize / 2
        style.left = (from.x + to.x + w) / 2
        style.top  = (from.y + to.y + w) / 2
        style.height = w
        style.width  = w
        dom.appendChild(it)
        gui.questionNew(it,q)
      }
    }

    const removeBonus = function(edge) {
      const el = placedBonuses[edge]
      delete placedBonuses[edge]
      el.remove()
    }


    // Initialize board.
    const edgeNodes = {}
    for (const i in opts.edges) {
      edgeNodes[i] = opts.geo[i]
      const edge = opts.edges[i]
      if (edge.bonus) placeBonus(i,edge.bonus)

      const workers = edge.workers
      for (const j in workers) placeWorker(i,j,workers[j])
    }


    // exported:
    ui.edgeNodes = edgeNodes
    ui.placeWorkerOnEdge = placeWorker
    ui.askEmptyEdgeSpot  = askEmptyWorker
    ui.askFullEdgeSpot   = askFullWorker
    ui.removeWorkerFromEdge = removeWorker

    ui.askEdge = askEdge
    ui.placeBonus        = placeBonus
    ui.removeBonus       = removeBonus
  }


  { // endVP

    // first spot is 1 (required priv)
    const placeWorker = function(spot, worker) {
      const loc = board.ptsSpot(spot)
      const el = drawWorkerAt(loc, board.workerSize, worker)
      dom.appendChild(el)
    }

    const askWorker = function(level,q) {
      const loc = board.ptsSpot(level)
      const worker = { shape: 'Disc', owner: playerId }
      const el = drawWorkerAt(loc, board.workerSize, worker)
      el.classList.add('empty')
      dom.append(el)
      gui.questionNew(el,q)
    }


    // exported:
    ui.placeWorkerOnVP = placeWorker
    ui.askWorkerOnVP = askWorker
  }

  { // upgrade actions
    const askUpgrade = function (node,stat,q) {
      const loc = board.upgradeSpot(stat)
      const el = document.createElement('div')
      el.classList.add('upgrade-action')
      const style = el.style
      style.width  = board.upgradeSize
      style.height = board.upgradeSize
      style.left   = loc.x
      style.top    = loc.y
      dom.appendChild(el)
      gui.questionNew(el,q)
    }

    ui.askUpgrade = askUpgrade

  }

  ui.appendChild = function(x) { dom.appendChild(x) }

  gui.container.appendChild(dom)
  return ui
}


