let gui = {}
const iconSize = 28

const toSize = (n) => {
  const scale = 1
  return scale * n
}

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




function uiRedraw(state) {
  gui = {}
  gui.questions = []

  const game = state.game
  const body = document.getElementById('main')

  gui.mainPanel = div('mainPanel')
  body.appendChild(gui.mainPanel)

  gui.turn = uiTurn(game._gameTurn)
  gui.mainPanel.appendChild(gui.turn.dom)

  gui.board = uiBoard(state.game._gameBoard)
  gui.mainPanel.appendChild(gui.board.dom)


  let drawNext = 0
  const turnOrder = game.gameTurnOrder
  const playerNum = turnOrder.length
  for (; drawNext < playerNum; ++drawNext) {
    if (turnOrder[drawNext] == playerId) break
  }
  if (drawNext >= playerNum) drawNext = 0

  const ps = {}
  gui.player = ps
  const players = game._gamePlayers
  for (let todo = playerNum; todo > 0; --todo) {
    const p = turnOrder[drawNext]
    ps[p] = uiPlayer(p, players[p])
    body.appendChild(ps[p].dom)
    ++drawNext
    if (drawNext >= playerNum) drawNext = 0
  }

  uiQuestions(state.questions)
}

const uiUpdate = hsUpdate (
  { PlaceCube: (pid,loc,r) => {
      gui.player[pid]
         .techs.tech[loc.cubeTech]
         .alt[loc.cubeAlt]
         .spot[loc.cubeSpot]
         .add(r)
    }
  , RemoveCube: (pid,loc) => {
      gui.player[pid]
         .techs.tech[loc.cubeTech]
         .alt[loc.cubeAlt]
         .spot[loc.cubeSpot]
         .remove()
    }
  , ChangeBag: (pid,nm,r,n)     => gui.player[pid].bag[nm].change(r,n)
  , ChangeGems: (pid,n)         => gui.player[pid].stats.changeGems(n)
  , ChangeGhosts: (pid,n)       => gui.player[pid].stats.changeGhosts(n)
  , ChangeWorkers: (pid,n)      => gui.player[pid].stats.changeWorkers(n)
  , Capture: (pid,capt)         => gui.player[pid].stats.capture(capt)
  , SetTurn: (t)                => gui.turn.redraw(t)
  , Upgrade: (pid,r,n)          => gui.player[pid].upgrade.change(r,n)
  , ResetUpgrade: (pid,r)       => gui.player[pid].upgrade.reset(r)
  , ChangeUnit: (pid,ty,loc,n)  => gui.board.changeUnit(loc,pid,ty,n)
  , SetUnithighlight: (loc,pid,yes) => gui.board.setUnitHighlight(loc,pid,yes)
  , SetCity: (loc,cityId,spot)  => { console.log(spot); gui.board.setCity(loc,cityId,spot) }
  , SetRuin: (loc,ruinId,spot)  => gui.board.setCity(loc,ruinId,spot)
  , ChangeTile: (loc,t) => gui.board.changeTile(loc,t)
  })
