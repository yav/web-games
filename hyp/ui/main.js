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
  body.appendChild(uiTurn(game._gameTurn))

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

function uiUpdate() {
  console.log('update')
}
