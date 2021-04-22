
function drawTurn(turn) {
  const ui = {}

  let done    = turn.actDone
  let limit   = turn.actLimit
  let bonuses = turn.bonuses

  const lab = function(l) {
    const it = document.createElement('span')
    it.classList.add('turn-label')
    it.textContent = l
    return it
  }

  const group = function(xs) {
    const g = document.createElement('span')
    g.classList.add('turn-group')
    for (let i = 0; i < xs.length; ++i) {
      g.appendChild(xs[i])
    }
    dom.appendChild(g)
    return g
  }

  const dom = document.createElement('div')
  dom.classList.add('turn')
  dom.style.fontSize = gui.board.fontSize

  { // scoring help
    const el = document.createElement('div')
    el.classList.add('undo')
    el.classList.add('question')
    el.textContent = '?'
    dom.appendChild(el)
    gui.tooltip(el,'Scoring Help')

    const help = document.createElement('div')
    help.classList.add('score')
    help.classList.add('help')
    help.style.display = 'none'
    const entry = function(l) {
      const x = document.createElement('div')
      x.innerHTML = l
      help.appendChild(x)
    }
    entry('<b><u>End Game Scoring</u></b>')
    entry('4 VP per max upgrade (not Keys)')
    entry('#Bonuses: 1,3,3,6,6,10,10,15,15,21 VP')
    entry('Workers on special action: 7,8,9,11 VP')
    entry('2 VP per controlled city')
    entry('Keys &#215; #offices in largest network')
    entry('Most controlled cities in a province<br>(#offices break ties)')
    dom.appendChild(help)

    let visible = false

    el.addEventListener('click',function() {
      help.style.display = visible ? 'none' : 'inline-block'
      visible = !visible
    })

  }

  { // undo btn
    const btn = document.createElement('span')
    btn.classList.add('question')
    btn.classList.add('undo')
    btn.innerHTML = '&#x2190;'
    btn.addEventListener('click',function() { gui.undo() })
    dom.appendChild(btn)
    gui.tooltip(btn,'Undo')
  }

  // placing a token at end of turn?
  gui.setPlacing(turn.placing)


  const player = lab(turn.player)
  player.classList.add('turn-player')
  player.classList.add(gui.colors[turn.player])
  group([ lab('Turn: '), player ])

  const elDone  = lab(done)
  const elLimit = lab(limit)
  group([ lab('Actions: '), elDone, lab('/'), elLimit ])

  const drawPicked = function(w) {
    const it = drawWorker(gui.board.workerSize,w)
    it.classList.add('hand')
    return it
  }

  const picked = []
  for (let i = 0; i < turn.pickedUp.length; ++i) {
    const w = turn.pickedUp[i]
    const it = drawPicked(w)
    picked[i] = it
  }
  const pickedGroup = group(picked)

  const question = document.createElement('div')
  question.classList.add('question-status')
  question.textContent = 'Question?'
  question.style.position = 'absolute'
  question.style.bottom = '-1.3em'
  dom.appendChild(question)

  gui.board.appendChild(dom)


  // exported
  return {

    setQuestion: (q) => {
      question.textContent = q
      question.style.display = 'inline-block'
    }

    , hideQuestion: () => {
        question.style.display = 'none'
      }

    , changeDone:
      function(n) {
        done = done + n
        elDone.textContent = done
      }

  , changeLimit:
      function(n) {
        limit = limit + n
        elLimit.textContent = limit
      }

  , askDone:
      function(text,json) {
        const btn = document.createElement('span')
        btn.classList.add('turn-player')
        btn.classList.add(gui.colors[turn.player])
        btn.textContent = text
        gui.questionNew(group([btn]), json)
      }

  , remove:
      function() { dom.remove() }

  , removeWorkerFromHand:
      function() {
        const ch = pickedGroup.children[0]
        pickedGroup.removeChild(ch)
      }
  , addWorkerToHand:
      function(mbProv,w) { pickedGroup.appendChild(drawPicked(w)) }

  }


}


