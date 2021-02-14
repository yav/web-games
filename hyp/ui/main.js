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
  const test = game.test
  for(let i = 0; i < test.length; ++i) {
    const ds = test[i]
    for (let j = 0; j < ds.length; ++j) {
      body.appendChild(uiTech(ds[j]))
      body.appendChild(document.createElement('br'))
    }
    body.appendChild(document.createElement('br'))
    body.appendChild(document.createElement('br'))
  }

}

function uiQuestions(questions) {
  console.log('questions')
}

function uiUpdate() {
  console.log('update')
}
