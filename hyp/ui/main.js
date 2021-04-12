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




const uiRedraw = (state) => {
  gui = {}
  gui.questions = []

  const game = state.game
  const body = document.getElementById('main')
  body.innerHTML = ""

  const mainPanel = div('mainPanel')
  body.appendChild(mainPanel)

  const topBar = div('top-bar')
  mainPanel.appendChild(topBar)

  gui.turn = uiTurn(game._gameTurn)
  topBar.appendChild(gui.turn.dom)

  gui.supply = uiBag('Supply', game._gameSupply)
  topBar.appendChild(gui.supply.dom)

  gui.board = uiBoard(state.game._gameBoard)
  mainPanel.appendChild(gui.board.dom)

  const ps = uiPlayers(game.gameTurnOrder, game._gamePlayers)
  gui.player = ps.players
  body.appendChild(ps.dom)

  gui.markets = uiMarkets(game._gameMarkets)
  mainPanel.appendChild(gui.markets.dom)

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
  , SetRuin: (loc,ruinId,spot)  => gui.board.setRuin(loc,ruinId,spot)
  , DropToken : (loc,ruinId)    => gui.board.dropToken(loc,ruinId)
  , ChangeTile: (loc,t)         => gui.board.changeTile(loc,t)
  , SetMarket: (d,m)            => gui.markets.setOffers(d,m)
  , AddTech: (pid,tid,t)        => gui.player[pid].techs.add(tid,t)
  , ChangeSupply: (r,n)         => gui.supply.change(r,n)
  , SetRuinToken: (pid,mb)      => gui.player[pid].stats.setToken(mb)
  , GainAchievement: (pid,a)    => gui.player[pid].stats.addAchievement(a)
  })
