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
  const body = document.getElementById('main')
  body.appendChild(chieftanToken(playerId))
  body.appendChild(explorerToken(playerId))
  body.appendChild(swordToken(playerId))
  body.appendChild(boatToken(playerId))
  body.appendChild(cartToken(playerId))
  body.appendChild(flagToken(playerId))

  body.appendChild(bisonToken())
  body.appendChild(leatherToken())
  body.appendChild(woodToken())
  body.appendChild(lumberToken())
  body.appendChild(oreToken())
  body.appendChild(metalToken())
  body.appendChild(stoneToken())
  body.appendChild(blockToken())
  uiQuestions(state.questions)
}

const uiQuestion = (q) => hsInput({
  })(q.chChoice)


const uiUpdate = hsUpdate({
  })


