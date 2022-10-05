let sendJSON = null
let playerId = null
let gui      = null

function main() {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) iconSize = conn.size
}

// Redraw the whole state
function uiRedraw (state) {
  const b = html.getBody()
  b.appendChild(drawGame(state.game))
  setGame(state.game)
  uiQuestions(state.questions)
}

// Set the explanation for what we are asking.
function uiSetQuestion(q) { gui.question.set(q) }

// Various things that can be used to answer the question.
const uiQuestion = (q) => hsInput({
  Deck: (i)   => gui.decks[i].ask(q),
  Hand: (i)   => gui.hand[i].ask(q),
  Text: (i,j) => makeTextQuestion(j,q)
  })(q.chChoice)


// Perform a partial update
const uiUpdate = setGame


