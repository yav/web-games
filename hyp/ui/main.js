function toSize(n) {
  const scale = 1
  return scale * n
}

let gui = {}

function uiRedraw(state) {
  console.log('redraw')
  gui = {}
  const body = document.getElementById('main')
  body.appendChild(uiBoard(state.game.gameBoard))

/*
  const test = state.game.test
  for(let i = 0; i < test.length; ++i) {
    body.appendChild(uiTech(test[i]))
    body.appendChild(document.createElement('br'))
  }
*/
}

function uiQuestions(questions) {
  console.log('questions')
}

function uiUpdate() {
  console.log('update')
}
