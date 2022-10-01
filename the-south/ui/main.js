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
  console.log("drew state")
  uiUpdate(state.game)
  uiQuestions(state.questions)
}

// Set the explanation for what we are asking.
function uiSetQuestion(q) {
  const dom = html.div("phrase")
  dom.textContent = q
  gui.question.appendChild(dom)
}

// Various things that can be used to answer the question.
const uiQuestion = (q) => hsInput({
  Deck: (i) => makeQuestion(gui.decks[i],q),
  Hand: (i) => makeQuestion(gui.cards[i],q),
  Text: (i,j) => makeTextQuestion(j,q)
  })(q.chChoice)


// Perform a partial update
function uiUpdate(game) {

  gui = {}

  const b = html.getBody()
  b.replaceChildren([])

  gui.question = html.div("question")
  b.appendChild(gui.question)
  b.appendChild(drawGame(game))
}


