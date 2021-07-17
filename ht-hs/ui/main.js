let playerId
let gui
let boardSize = 800
let sendJSON

const uiUpdate = hsGameUpdate(
  {
    // Edges
    PlaceWorkerOnEdge:    function(...as) { gui.board.placeWorkerOnEdge(...as) }
  , RemoveWorkerFromEdge: function(...as) { gui.board.removeWorkerFromEdge(...as)
    }
  , EdgeRemoveBonus:      function(...as) { gui.board.removeBonus(...as) }
  , EdgeSetBonus:         function(...as) { gui.board.placeBonus(...as) }

    // Nodes
  , PlaceWorkerInOffice:  function(...as) { gui.board.placeWorkerInOffice(...as) }
  , PlaceWorkerInAnnex:   function(...as) { gui.board.placeWorkerInAnnex(...as) }
  , SwapWorkers:          function(...as) { gui.board.swapWorkers(...as) }
  , SetEndVPAt:           function(...as) { gui.board.placeWorkerOnVP(...as) }

    // misc
  , SetFull: function(...as) { gui.board.setFull(...as) }

    // player
  , SetWorkerPreference: function (w) {
      gui.playerUI(w.owner).setPreference(w.shape)
    }
  , ChangeAvailble: function(w,n) {
      gui.playerUI(w.owner).changeWorkers('available',w.shape,n)
    }
  , ChangeUnavailable: function(w,n) {
      gui.playerUI(w.owner).changeWorkers('unavailable',w.shape,n)
    }
  , UseGateway: function(g) {}
  , ChangeVP: function(player,n) { gui.playerUI(player).changeVP(n) }
  , Upgrade: function(player,stat) { gui.playerUI(player).upgrade(stat) }
  , GainBonusToken: function(player,token) { gui.playerUI(player).addBonus(token) }
  , UseBonusToken: function(player,bonus) {
      const ui = gui.playerUI(player)
      ui.removeBonus(bonus)
      ui.addSpentBonus()
    }


    // turn
  , NewTurn: function(t) { gui.turn.remove(); gui.turn = drawTurn(t) }
  , ChangeDoneActions: function(...as) { gui.turn.changeDone(...as) }
  , ChangeActionLimit: function(...as) { gui.turn.changeLimit(...as) }
  , AddWorkerToHand:   function(...as) { gui.turn.addWorkerToHand(...as) }
  , RemoveWorkerFromHand: function(...as) { gui.turn.removeWorkerFromHand(...as) }
  , DrawBonusToken: function() { gui.changeTokenCount(-1) }
  , PlacingBonus: function(...as) { gui.setPlacing(...as) }
  , AchieveBonusRoute: function() {}

  // log
  , Log: function(...as) { gui.log.addLog(...as) }

  , EndGame: function() { gui.reload() }
  }
)




function main() {
  const conn = srvConnect()
  playerId = conn.playerId
  if (conn.size) boardSize = conn.size
  sendJSON = conn.sendJSON
}


function newGUI(container) {
  container.innerHTML = ''

  const questionsExtra = []
  const questionsElems = []
  const ui = {}

  const newQuestionExtra = function(d) {
    questionsExtra[questionsExtra.length] = d
  }

  const removeQuestions = function() {
    for (let i = 0; i < questionsExtra.length; ++i) questionsExtra[i].remove()
    for (let i = 0; i < questionsElems.length; ++i) questionsElems[i].rm()
    gui.playerUI().hideActionIndicator()
    gui.turn.hideQuestion()
  }

  ui.highlightQuestions = function() {
    for (let i = 0; i < questionsExtra.length; ++i)
      questionsExtra[i].classList.add('large')
    for (let i = 0; i < questionsElems.length; ++i)
      questionsElems[i].dom.classList.add('large')
  }

  ui.unhighlightQuestions = function() {
    for (let i = 0; i < questionsExtra.length; ++i)
      questionsExtra[i].classList.remove('large')
    for (let i = 0; i < questionsElems.length; ++i)
      questionsElems[i].dom.classList.remove('large')
  }



  const tooltip = function(el,lab) {
    const tip = document.createElement('div')
    tip.classList.add('tooltip')
    tip.textContent = lab
    tip.style.left = el.offsetLeft + 20
    tip.style.top  = el.offsetTop + 20
    el.parentNode.appendChild(tip)

    const funEnter = function(ev) { tip.style.display = 'inline-block' }
    const funLeave = function(ev) { tip.style.display = 'none' }
    el.addEventListener('mouseenter',funEnter)
    el.addEventListener('mouseleave',funLeave)

    return { dom: tip, enter: funEnter, leave: funLeave }
  }

  const makeQuestion = function(el,val) {
    const tip = tooltip(el,val.chHelp)
    newQuestionExtra(tip.dom)

    // place them in the same parent so that z-indexes work correctly
    el.classList.add('question')

    const funClick = function(ev) {
      removeQuestions()
      console.log('sending:')
      console.log(val)
      sendJSON(val)
    }
    el.addEventListener('click',funClick)
    return { dom: el, rm: function() {
      el.classList.remove('question')
      el.removeEventListener('click',funClick)
      el.removeEventListener('mouseenter',tip.enter)
      el.removeEventListener('mouseleave',tip.leave)
    }}
  }

  ui.tooltip = tooltip

  ui.questionAnnot = function(el,val) {
    questionsElems[questionsElems.length] = makeQuestion(el,val)
    gui.playerUI().showActionIndicator()
  }

  ui.questionNew = function(el,val) {
    newQuestionExtra(el)
    makeQuestion(el,val)
    gui.playerUI().showActionIndicator()
  }

  ui.container = container

  ui.undo = function() {
    const msg = { tag: 'undo' }
    console.log('sending:')
    console.log(msg)
    sendJSON(msg)
  }

  ui.reload = function () { sendJSON({ tag: 'reload' }) }

  return ui
}


function uiRedraw(state) {
  gui = newGUI(document.getElementById('main'))

  const game = state.game ? state.game : state.finished

  { // Colors
    gui.colors = {}
    for (let i = 0; i < game.turnOrder.length; ++i) {
      const pid = game.turnOrder[i]
      gui.colors[pid] = playerColors[pid]
    }
  }


  { // Board
    const board = game.board
    board.size = boardSize
    gui.board = drawBoard(board)
  }

  { // Token count
    const el = document.createElement('div')
    el.classList.add('token-count')
    const loc = gui.board.tokenCountSpot
    const style = el.style
    style.left = loc.x
    style.top  = loc.y

    let amt = game.tokens
    const lab = document.createElement('div')
    const sz = gui.board.bonusSize
    lab.classList.add('label')
    lab.textContent = amt
    el.appendChild(lab)
    gui.tooltip(lab,'Remaining bonus tokens')
    gui.board.appendChild(el)

    let placing = null

    gui.changeTokenCount = function(n) {
      amt = amt + n
      if (amt < 0) amt = 0
      lab.textContent = amt
    }
    gui.setPlacing = function(mb) {
      if (placing) placing.remove()
      if (mb) {
        if (placing) placing.remove()
        placing = drawBonusToken(gui.board.bonusSize,mb)
        el.appendChild(placing)
        gui.tooltip(placing,'Place this token')
      } else {
        placing = null
      }
    }
  }



  { // Players
    const height = boardSize / 6.5
    const width  = 3 * height

    gui.players = {}
    const cont = document.createElement('div')
    gui.playerContainer = cont
    cont.classList.add('player-container')
    gui.container.appendChild(cont)

    let start = 0
    for (let i = 0; i < game.turnOrder.length; ++i) {
      if (game.turnOrder[i] === playerId) { start = i; break }
    }

    for (let i = 0; i < game.turnOrder.length; ++i) {
      const pid = game.turnOrder[(start + i) % game.turnOrder.length]
      const s   = game.players[pid]
      s.height  = height
      s.width   = width
      s.name    = pid
      const p = drawPlayer(pid,s)
      gui[pid] = p
    }
    gui.playerUI = function(x) { return gui[x ? x : playerId] }
  }

  // End VP
  for (const i in game.endVP) gui.board.placeWorkerOnVP(i, game.endVP[i])

  gui.panel = document.createElement('div')
  gui.panel.classList.add('panel')
  gui.container.appendChild(gui.panel)


  let score = null
  { // Current turn
    const stat = game.status
    if (stat.tag === 'finished') {
      score = drawScore(game.turnOrder, game.score)
    } else
      gui.turn = drawTurn(game.status)
  }

  { // Log
    gui.log = drawLog()
    const n = game.log.length
    for (let i = n-1; i >= 0; --i) {
      gui.log.addLog(game.log[i])
    }
    if (score) {
      gui.log.prepend(score)
    }
  }



  // questions
  uiQuestions(state.questions)
}

const uiSetQuestion = (q) => {
  console.log(q)
  gui.turn.setQuestion(q)
}

const uiQuestion = (q) => {
  const ui = {}

  // Player
  ui.ChSetPreference = function(shape) {
    gui.playerUI().askWorker('available',shape,q)
  }
  ui.ChActiveWorker = function(shape) {
    gui.playerUI().askWorker('available',shape,q)
  }
  ui.ChPassiveWorker = function(shape) {
    gui.playerUI().askWorker('unavailable',shape,q)
  }
  ui.ChBonusToken = function(bonus) { gui.playerUI().askBonus(bonus,q) }
  ui.ChUpgrade = function(stat) { gui.playerUI().askUpgrade(stat,q) }

  // Edges
  ui.ChEdgeEmpty = function(edge,spot,shape) {
    gui.board.askEmptyEdgeSpot(edge,spot,shape,q)
  }
  ui.ChEdgeFull = function(edge,spot,mbShape,worker) {
    gui.board.askFullEdgeSpot(edge,spot,mbShape,worker,q)
  }
  ui.ChEdge = function(edge) { gui.board.askEdge(edge,q) }

  // Nodes
  ui.ChNodeEmpty = function(node,shape) {
    gui.board.askEmptyOffice(node,shape,q)
  }
  ui.ChNodeAnnex = function(node,shape) { gui.board.askAnnex(node,shape,q) }

  ui.ChNodeFull = function(node,spot) { gui.board.askFullOffice(node,spot,q) }
  ui.ChNodeUpgrade = function(node,stat) { gui.board.askUpgrade(node,stat,q) }
  ui.ChEndVPSpot = function(level) { gui.board.askWorkerOnVP(level,q) }

  // Button
  ui.ChDone = function(text) { gui.turn.askDone(text,q) }

  hsChoice(ui)(q.chChoice)
}



