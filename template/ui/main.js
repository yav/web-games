let sendJSON = null
let playerId = null
let iconSize = 26
let gui      = null

const main = () => {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) iconSize = conn.size
}

const uiRedraw = (state) => {
  uiQuestions(state.questions)
}

const uiQuestion = (q) => hsInput({
  })(q.chChoice)


const uiUpdate = hsUpdate({
  })


