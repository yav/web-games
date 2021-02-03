function toSize(n) {
  const scale = 1
  return scale * n
}

let gui = {}

function uiRedraw(state) {
  console.log('redraw')
  gui = {}
  gui.container = document.getElementById('main')
  uiBasicAction(state.game.test)
}

function uiQuestions(questions) {
  console.log('questions')
}

function uiUpdate() {
  console.log('update')
}
