const removeQuestions = () => {
  const qs = gui.questions
  for (let i = 0; i < qs.length; ++i)
    qs[i]()
  gui.questions = []
}

const existingQuestion = (el,above,q) => {
  el.classList.add('question')
  const help = tooltip(el,above,q.chHelp)

  const onClick = () => {
    removeQuestions()
    sendJSON(q)
  }

  gui.questions[gui.questions.length] = () => {
    help.remove()
    el.classList.remove('question')
    el.removeEventListener('click',onClick)
  }

  el.addEventListener('click',onClick)
}

const newQuestion = (el,q,fin) => {
  el.classList.add('question')
  gui.questions[gui.questions.length] = () => {
    el.remove()
    if(fin) fin()
  }

  el.addEventListener('click',() => {
    removeQuestions()
    sendJSON(q)
  })
}

// XXX
const uiSetQuestion = () => {}

const uiQuestion = (q) => {
  hsInput({
    AskCubeLoc: (x) => {
      const dom = gui.player[playerId]
                     .techs.tech[x.cubeTech].alt[x.cubeAlt].spot[x.cubeSpot].dom
      existingQuestion(dom,false,q)
    },
    AskReady: (r) => gui.player[playerId].bag.BagReady.ask(r,q),
    AskDiscard: (r) => gui.player[playerId].bag.BagDiscard.ask(r,q),
    AskSupply: (r) => gui.supply.ask(r,q),
    AskPlayerToken: () => gui.player[playerId].stats.askToken(q),
    AskButton: (txt) => {
      const btn = div('group button')
      btn.textContent = txt
      newQuestion(btn,q)
      gui.turn.dom.appendChild(btn)
    },
    AskUpgrade: (c)         => gui.player[playerId].upgrade.ask(c,q),
    AskReadyAction: (r)     => gui.turn.askBasic(r,q),
    AskIfAction: (n)        => gui.turn.askIf(n,q),
    AskOrActionLeft: (n)    => gui.turn.askOrLeft(n,q),
    AskOrActionRight: (n)   => gui.turn.askOrRight(n,q),
    AskCity: (loc,id)       => gui.board.askCity(loc,id,q),
    AskRuin: (loc,id)       => gui.board.askRuin(loc,id,q),
    AskUnit: (loc,pid)      => gui.board.askUnit(loc,pid,q),
    AskMap: (loc,a)         => gui.board.askMap(loc,a,q),
    AskMarketItem: (d,n)    => gui.markets.askMarketItem(d,n,q),
    AskMarketDeck: (d)      => gui.markets.askMarketDeck(d,q),
    AskPlayerTech: (pid,t)  => gui.player[pid].techs.ask(t,q)
  })(q.chChoice)

}


