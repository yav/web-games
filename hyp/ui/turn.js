
const uiTurn = (tu) => {
  const dom = div('turn')

  let sReady = {}
  let sIfs   = {}
  let sOrs   = {}

  const actKey = (a) => JSON.stringify(a)

  const draw = (t) => {
    const undo = div('button question group')
    undo.innerHTML = '&#x2190;'
    undo.addEventListener('click', () => sendJSON({tag: 'undo'}))
    tooltip(undo,false,'Undo')
    dom.appendChild(undo)

    const lab = div('group')
    lab.appendChild(span('Turn: '))
    lab.appendChild(uiPlayerBadge(t.turnPlayer))
    dom.appendChild(lab)
    basic = {}
    const ready = t._turnReady
    for (let i = 0; i < ready.length; ++i) {
      const an = ready[i]
      const r = actKey(an[0])
      const it = uiBasicAction({tag:'Times',contents:ready[i]})
      sReady[r] = it
      it.classList.add('group')
      dom.appendChild(it)
    }

    const ifs = t._turnIfs
    for (let i = 0; i < ifs.length; ++i) {
      const it = uiAction({tag:'If',contents:ifs[i]})
      sIfs[i] = it
      it.dom.classList.add('group')
      dom.appendChild(it.dom)
    }

    const ors = t._turnOrs
    for (let i = 0; i < ors.length; ++i) {
      const it = uiAction({tag:'Or',contents:ors[i]})
      sOrs[i] = it
      it.dom.classList.add('group')
      dom.appendChild(it.dom)
    }

  }

  draw(tu)

  return {
    dom: dom,
    redraw: (t) => {
      dom.innerHTML = ''
      draw(t)
    },
    askBasic: (r,q) => { existingQuestion(sReady[actKey(r)],true,q) },
    askIf: (n,q) => { existingQuestion(sIfs[n].left,true,q) },
    askOrLeft: (n,q) => { existingQuestion(sOrs[n].left,true,q) },
    askOrRight: (n,q) => { existingQuestion(sOrs[n].right,true,q) }
  }
}
