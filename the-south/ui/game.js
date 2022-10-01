
function makeQuestion(dom,q) {
  dom.classList.add("answer")
  dom.setAttribute("title",q.chHelp)
  dom.addEventListener("click", () => sendJSON(q))
}

function makeTextQuestion(txt,q) {
  const dom = html.div("text-answer answer")
  dom.textContent = txt
  dom.setAttribute("title",q.chHelp)
  dom.addEventListener("click", () => sendJSON(q))
  gui.question.appendChild(dom)
}



function drawGame(game) {
  const dom = html.div('game')
  dom.appendChild(drawDecks(game))
  dom.appendChild(drawPlayers(game))

  return dom
}

function drawDecks(state) {
  const ds = state.decks
  const dom = html.div("decks")
  gui.decks = []
  for (let i = 0; i < ds.length; ++i) {
    dom.appendChild(drawDeck(i,ds[i]))
  }

  dom.appendChild(drawDiscard(state.discard))

  return dom
}


function drawDeck(i,d) {
  const dom = html.div("deck")
  switch (d.tag) {
    case 'Empty':
      dom.textContent = "(empty)"
      break

    case 'Card':
      const ancient = drawCard(d.contents[0])
      const counter = html.div("counter")
      counter.textContent = (1 + d.contents[1]) + ""
      dom.appendChild(ancient)
      dom.appendChild(counter)
      break
  }
  gui.decks[i] = dom
  return dom
}

function drawDiscard(n) {
  const dom = html.div("discard")
  dom.setAttribute("title","Discard")
  const d   = drawCard("Blank")
  const c   = html.div("counter")
  c.textContent = n + ""
  dom.appendChild(d)
  dom.appendChild(c)
  return dom
}


function drawPlayers(game) {
  const ps = game.players

  const dom = html.div("players")

  const players = []
  for (const p in ps) {
    players[p] = drawPlayer(p, ps[p])
    dom.appendChild(players[p])
  }

  return dom
}

function drawPlayer(p, state) {
  const dom = html.div("player")
  const label = html.div("label")
  label.textContent = p + (state.isLast ? "(2nd)" : "")
  if (state.isCurrent) {
    label.classList.add("current")
  }

  const stats = html.div("stats")
  stats.textContent = "points: " + state.points + ", valut: " + state.vault +
                      ", salvos: " + state.salvos

  const hand = drawHand(state.hand)

  dom.appendChild(label)
  dom.appendChild(stats)
  dom.appendChild(hand)

  return dom
}

function drawHand(hand) {
  switch(hand.tag) {
    case "Opaque": {
      const dom = html.div("hand opaque")
      dom.textContent = hand.contents + " cards in hand"
      return dom
    }

    case "Visible": {
      const dom = html.div("hand visible")
      const cs = hand.contents
      gui.cards = []
      for (let i = 0; i < cs.length; ++i) {
        gui.cards[i] = drawCard(cs[i])
        dom.appendChild(gui.cards[i])
      }
      return dom
    }
    default:
      const dom = html.div("error")
      dom.textContent = "Unexpected: " + hand.tag
      return dom
  }
}

function drawCard(i) {
  const dom = html.div("card")
  const img = html.img("image/" + i + ".png")
  html.setDim(img, 128, 196)
  dom.appendChild(img)
  return dom
}
