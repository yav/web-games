const removeQuestions = () => {
  const qs = gui.questions
  for (let i = 0; i < qs.length; ++i)
    qs[i]()
  gui.questions = []
}

const existingQuestion = (el,q) => {
  el.classList.add('question')

  const onClick = () => {
    removeQuestions()
    sendJSON(q)
  }

  gui.questions[gui.questions.length] = () => {
    el.classList.remove('question')
    el.removeEventListener('click',onClick)
  }

  el.addEventListener('click',onClick)
}

const uiQuestion = (q) => {
  hsInput({
    AskCubeLoc: (x) => {
      const dom = gui.player[playerId]
                     .techs.tech[x.cubeTech].alt[x.cubeAlt].spot[x.cubeSpot]
      existingQuestion(dom,q)
    }
  })(q.chChoice)

}

const uiQuestions = (qs) => {
  for (let i = 0; i < qs.length; ++i) {
    uiQuestion(qs[i])
  }
}


