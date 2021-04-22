function drawLog() {
  const dom = document.createElement('div')
  dom.classList.add('log')
  dom.style.fontSize = gui.board.fontSize
  gui.panel.appendChild(dom)

  let turnBox = null
  let actionBox = null
  let box

  const lab = function(x,cl) {
    const el = document.createElement('span')
    el.textContent = x
    if (cl !== undefined)
      for (let i = 0; i < cl.length; ++i)
        el.classList.add(cl[i])
    box.appendChild(el)
    return el
  }

  const sayNode = function(nodeId,spot) {
    let msg = gui.board.nodeNames[nodeId]
    if (spot !== undefined && spot !== null) {
      msg = msg + ', office ' + (spot+1)
    }
    const el = lab(msg,['log-unit'])
    el.addEventListener('mouseenter',function(){gui.board.hilightNode(nodeId)})
    el.addEventListener('mouseleave',
                          function(){gui.board.unhilightNode(nodeId)})
  }

  const sayEdge = function(edgeId,spot) {
    const nodes = gui.board.edgeNodes[edgeId]
    sayNode(nodes[0])
    lab('-')
    sayNode(nodes[1])
    if (spot !== undefined && spot !== null) {
      lab(', spot ' + spot)
    }
/*
    const from  = gui.board.nodeNames[nodes[0]]
    const to    = gui.board.nodeNames[nodes[1]]
    let msg = from + '-' + to
    if (spot !== undefined && spot !== null) {
      msg = msg + ', spot ' + spot
    }
    lab(msg,['log-unit'])
*/
  }

  const sayWorker = function(worker) {
    box.appendChild(drawWorker(gui.board.workerSize,worker))
  }

  const sayPlayer = function(player) {
    lab(player, [ 'turn-player', gui.colors[player]])
  }

  const sayBonus = function(bonus) {
    box.appendChild(drawBonusToken(gui.board.bonusSize,bonus))
  }

  const sayEl = hsEventElement({
     EvText: lab,
     EvInt: lab,
     EvPlayer: sayPlayer,
     EvWorker: sayWorker,
     EvEdge: sayEdge,
     EvNode: sayNode,
     EvBonus: sayBonus,
     EvStat: lab
   })



  const ui = {}
  ui.EvSay = function(xs) {
    for (let i = 0; i < xs.length; ++i) {
      sayEl(xs[i])
    }
    return true
  }

  ui.StartTurn = function() {
    turnBox = document.createElement('div')
    turnBox.classList.add('log-turn')
    dom.prepend(turnBox)
    return false
  }

  ui.EndTurn = function() {
    turnBox = null
    return false
  }

  ui.StartAction = function() {
    actionBox = document.createElement('div')
    actionBox.classList.add('log-action')
    const it = turnBox ? turnBox : dom
    it.appendChild(actionBox)
    return false
  }

  ui.EndAction = function() {
    actionBox = null
    return false
  }

  const handler = hsEvent(ui)

  return {
    addLog: function(msg) {

      box = document.createElement('div')
      box.classList.add('log-item')
      if (handler(msg)) {
        const el = actionBox ? actionBox
                 : (turnBox ? turnBox : dom)
        el.appendChild(box)
      }
    }

    , prepend: function(x) {
      dom.prepend(x)
    }
  }
}


