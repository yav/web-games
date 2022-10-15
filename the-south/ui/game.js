function questionClick(q) {
  return () => {
    sendJSON(q)
    const n = gui.questions.length
    for (let i = 0; i < n; ++i) gui.questions[i]()
    gui.questions = []
    gui.question.set('')
  }
}


function makeQuestion(dom,q) {
  dom.classList.add("answer")
  const oldTitle = dom.getAttribute("title")
  dom.setAttribute("title",q.chHelp)

  const click = questionClick(q)

  gui.questions.push(() => {
    dom.classList.remove("answer")
    dom.setAttribute("title",oldTitle)
    dom.removeEventListener("click", click)
  })

  dom.addEventListener("click", click)
}

function makeTextQuestion(txt,q) {
  const dom = html.div("text-answer answer")
  dom.textContent = txt
  dom.setAttribute("title",q.chHelp)
  gui.questions.push(() => dom.remove())

  dom.addEventListener("click", questionClick(q))
  gui.question.append(dom)
}


function setGame(game) {
  for (let i = 0; i < game.decks.length; ++i)
    gui.decks[i].set(game.decks[i])

  gui.discard(game.discard)

  for (const p in game.players)
    gui.players[p](game.players[p])
}


function drawGame(game) {
  gui = {}
  gui.questions = []
  const dom = html.div('game')
  dom.appendChild(drawQuestionArea())
  dom.appendChild(drawDecks(game.decks))
  dom.appendChild(drawPlayers(game.players))
  return dom
}

function drawQuestionArea() {
  let value = null
  const dom = html.div("question")
  const lab = html.div("phrase")
  dom.appendChild(lab)

  gui.question =
    { set: (i) => {
        if (value !== null && value.localeCompare(i) == 0) return
        lab.textContent = i
      }
    , append: (d) => dom.appendChild(d)
    }

  return dom
}


// Assumes the players do not change
function drawPlayers(ps) {
  gui.players = {}
  const dom = html.div("players")
  for (const p in ps)
    dom.appendChild(drawPlayer(p,ps[p]))
  return dom
}


function drawPlayer(p,st) {
  console.log(p)
  const dom   = html.div("player")
  const label = html.div("label")
  label.textContent = p + (st.isLast ? "(2nd)" : "")  // Doesn't change


  const stats  = html.div("stats")
  const points = drawCounter()
  const vault  = drawCounter()
  const salvos = drawCounter()
  const hand   = drawHand(st.hand)

  dom.append( label
            , "points: ", points.dom
            , ", vault:  ", vault.dom
            , ", salvos: ", salvos.dom
            , hand.dom
            )

  let current = null

  gui.players[p] = (s) => {
    if (s.isCurrent !== current) {
      current = s.isCurrent
      if (current)
        label.classList.add("current")
      else
        label.classList.remove("current")
    }
    points.set(s.points)
    vault.set(s.vault)
    salvos.set(s.salvos)
    hand.set(s.hand)
  }

  return dom
}


function drawHand(st) {
  gui.hand = []

  const dom = html.div("hand")
  const counter = drawCounter()

  if (st.tag === "Opaque") dom.append("hand: ", counter.dom)

  return { dom: dom
         , set: (hs) => {
             switch(hs.tag) {
               case "Visible":
                 const cs = hs.contents
                 for (let i = 0; i < cs.length; ++i) {
                   if (i >= gui.hand.length) {
                      gui.hand[i] = drawCard()
                      dom.appendChild(gui.hand[i].dom)
                   }
                   gui.hand[i].set(cs[i])
                 }
                 while(gui.hand.length > cs.length)
                   gui.hand.pop().remove()
                 return

              case "Opaque":
                counter.set(hs.contents)
                return
             }
           }
         }
}




// Assumes the number of decks does not change
function drawDecks(ds) {
  const dom = html.div("decks")
  gui.decks = []

  for (let i = 0; i < ds.length; ++i)
    dom.appendChild(drawDeck(i))

  dom.appendChild(drawDiscard())

  return dom
}


function drawDeck(i) {
  const dom     = html.div("deck")
  const card    = drawCard()
  const counter = drawCounter()
  dom.appendChild(card.dom)
  dom.appendChild(counter.dom)

  gui.decks[i] =
    { set: (v) => {
        switch (v.tag) {

          case "Empty":
            card.set('Blank')
            counter.set(0)
            return

          case "Card":
            card.set(v.contents[0])
            counter.set(v.contents[1] + 1)
            return
        }
      }

    , ask: (q) => makeQuestion(dom,q)
    }

  return dom
}



function drawDiscard() {
  const dom = html.div("deck")
  dom.setAttribute("title","Discarded cards")
  const card    = drawCard()
  const counter = drawCounter()
  dom.appendChild(card.dom)
  dom.appendChild(counter.dom)

  card.set("Blank")
  gui.discard = counter.set
  return dom
}




// -----------------------------------------------------------------------------
// Primitive components
// -----------------------------------------------------------------------------

function drawCard() {
  let value = null
  const dom = html.div("card")
  html.setDim(dom, 132, 196)

  return { dom: dom
         , set: (i) => {
                    if (i === value) return
                    if (value) dom.classList.remove(value)
                    value = i
                    if (value) dom.classList.add(value)
                  }
         , remove: () => dom.remove()
         , ask: (q) => makeQuestion(dom,q)
         }
}


function drawCounter() {
  let value = null
  const dom = html.div("counter")

  return { dom: dom
         , set: (n) => { if (n !== value) { value = n; dom.textContent = n }}
         }
}



