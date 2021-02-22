const removeQuestions = () => {
  const qs = gui.questions
  for (let i = 0; i < qs.length; ++i)
    qs[i]()
  gui.questions = []
}

const existingQuestion = (el,q) => {
  el.classList.add('question')
  const help = tooltip(el,false,q.chHelp)

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

const newQuestion = (el,q) => {
  el.classList.add('question')
  gui.questions[gui.questions.length] = () => { el.remove() }

  el.addEventListener('click',() => {
    removeQuestions()
    sendJSON(q)
  })
}

const uiQuestions = (qs) => {
  for (let i = 0; i < qs.length; ++i) {
    uiQuestion(qs[i])
  }
}

const uiQuestion = (q) => {
  hsInput({
    AskCubeLoc: (x) => {
      const dom = gui.player[playerId]
                     .techs.tech[x.cubeTech].alt[x.cubeAlt].spot[x.cubeSpot].dom
      existingQuestion(dom,q)
    },
    AskReady: (r) => {
      gui.player[playerId].bag.BagReady.ask(r,q)
    },
    AskButton: (txt) => {
      const btn = div('button')
      btn.textContent = txt
      newQuestion(btn,q)
      gui.menu.appendChild(btn)
    },
    AskUpgrade: (c) => { gui.player[playerId].upgrade.ask(c,q) },
    AskReadyAction: (r) => { gui.turn.askBasic(r,q) },
    AskIfAction: (n) => { gui.turn.askIf(n,q) },
    AskOrActionLeft: (n) => { gui.turn.askOrLeft(n,q) },
    AskOrActionRight: (n) => { gui.turn.askOrRight(n,q) },
  })(q.chChoice)

}


