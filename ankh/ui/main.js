let sendJSON = null
let playerId = null

function main() {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) html.setScale(conn.size)
}

// Redraw the whole state
function uiRedraw(state) {
  uiQuestions(state.questions)
  const body = html.getBody()
  for (let i = 0; i < 5; ++i)
    for (let j = 0; j < 5; ++j)
      body.appendChild(hex(i,j))

  body.appendChild(hexWall(3,2,0))
  body.appendChild(hexWall(3,2,1))
  body.appendChild(hexWall(3,2,2))
}

// Set the explanation for what we are asking.
function uiSetQuestion(q) {
}

// Various things that can be used to answer the question.
function uiQuestion(q) { return hsInput({
  })(q.chChoice)
}


// Perform a partial update
const uiUpdate = hsUpdate({
  })


