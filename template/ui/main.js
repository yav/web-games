let sendJSON = null
let playerId = null
let iconSize = 26
let gui      = null

function main () {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) iconSize = conn.size
}

// Redraw the whole state
function uiRedraw (state) {
  const body = html.getBody()
  body.innerHTML = ""
  gui = {}

  gui.counter = html.div("")
  gui.counter.textContent = "?"
  body.appendChild(gui.counter)

  gui.question = html.div("")
  body.appendChild(gui.question)

  uiUpdate(state.game)
  uiQuestions(state.questions)
}


// Set the explanation for what we are asking.
function uiSetQuestion (q) {
  gui.question.textContent = q
  gui.questions = []
}

// Various things that can be used to answer the question.
function uiQuestion (q) {
  const body = html.getBody()

  function btn(lab) {
    const dom = html.div("btn")
    dom.textContent = lab
    dom.addEventListener("click", () => {
      const n = gui.questions.length
      for (let i = 0; i < n; ++i) gui.questions[i].remove()
      gui.question.textContent = ""
      sendJSON(q)
    })
    body.appendChild(dom)
    gui.questions.push(dom)
  }

  hsInput({
    Inc: () => btn("inc"),
    Dec: () => btn("dec")
    })(q.chChoice)
}


function uiUpdate(newS) {
  gui.counter.textContent = newS[1]
}


