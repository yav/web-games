function toSize(n) {
  const scale = 1
  return scale * n
}

let gui = {}

function uiRedraw(state) {
  console.log('redraw')
  gui = {}
  const body = document.getElementById('main')

  console.log(state)

  const game = state.game
  body.appendChild(uiTurn(game._gameTurn))

  const players = game._gamePlayers
  for (p in players) {
    body.appendChild(uiPlayer(p, players[p]))
  }

/*
  body.appendChild(uiBoard(state.game.gameBoard))
*/

}

function uiQuestions(questions) {
  console.log('questions')
}

function uiUpdate() {
  console.log('update')
}
