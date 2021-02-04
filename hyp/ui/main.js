function toSize(n) {
  const scale = 1
  return scale * n
}

let gui = {}

function uiRedraw(state) {
  console.log('redraw')
  gui = {}
  gui.container = document.getElementById('main')

  const test = state.game.test
  for(let i = 0; i < test.length; ++i) {
    gui.container.appendChild(uiTech(test[i]))
    gui.container.appendChild(document.createElement('br'))
  }
}

function uiQuestions(questions) {
  console.log('questions')
}

function uiUpdate() {
  console.log('update')
}
