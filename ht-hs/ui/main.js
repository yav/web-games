let playerId
let gui
let boardSize = 800

function main() { srvConnect() }


function newGUI(ws,container) {
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
      ws.send(JSON.stringify(val))
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
    ws.send(JSON.stringify(msg))
  }

  ui.reload = function () { sendJSON(ws,{ tag: 'reload' }) }

  return ui
}


function uiRedraw(ws,state) {
  gui = newGUI(ws, document.getElementById('main'))

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


function uiQuestions(q_qs) {
  const qs = q_qs[1]
  gui.turn.setQuestion(q_qs[0])

  for (let i = 0; i < qs.length; ++i) {
    const q = qs[i]
    const ui = {}

    // Player
    ui.ChSetPreference = function(shape) {
      gui.playerUI().askWorker('available',shape,q)
      // gui.playerUI().askPreference(q)
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
}



