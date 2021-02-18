let gui = {}

const toSize = (n) => {
  const scale = 1
  return scale * n
}


function uiRedraw(state) {
  console.log('redraw')
  gui = {}
  gui.questions = []

  const body = document.getElementById('main')

  const game = state.game
  gui.turn = uiTurn(game._gameTurn)
  body.appendChild(gui.turn.dom)

  const players = game._gamePlayers
  const ps = {}
  gui.player = ps
  for (p in players) {
    ps[p] = uiPlayer(p, players[p])
    body.appendChild(ps[p].dom)
  }

/*
  body.appendChild(uiBoard(state.game.gameBoard))
*/

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
  , RemoveFromReady:  (pid,r) => { gui.player[pid].available.remove(r) }
  , AddToReady:       (pid,r) => { gui.player[pid].available.add(r) }
  , RemoveFromBag:    (pid,r) => { gui.player[pid].bag.remove(r) }
  , AddToBag:         (pid,r) => { gui.player[pid].bag.add(r) }
  , RemoveFromDiscard:(pid,r) => { gui.player[pid].discarded.remove(r) }
  , AddToDiscard:     (pid,r) => { gui.player[pid].discarded.add(r) }
  })
