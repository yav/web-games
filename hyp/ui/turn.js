
const uiTurn = (tu) => {
  const dom = div('turn')

  let sReady = {}
  let sIfs   = {}
  let sOrs   = {}

  const actKey = (a) => JSON.stringify(a)

  const draw = (t) => {
    dom.appendChild(span(tu.turnPlayer))
    basic = {}
    const ready = t._turnReady
    for (let i = 0; i < ready.length; ++i) {
      const an = ready[i]
      const r = actKey(an[0])
      sReady[r] = uiBasicAction({tag:'Times',contents:ready[i]})
      dom.appendChild(sReady[r])
    }

    const ifs = t._turnIfs
    for (let i = 0; i < ifs.length; ++i) {
      sIfs[i] = uiAction({tag:'If',contents:ifs[i]})
      dom.appendChild(sIfs[i].dom)
    }

    const ors = t._turnOrs
    for (let i = 0; i < ors.length; ++i) {
      sOrs[i] = uiAction({tag:'Or',contents:ors[i]})
      dom.appendChild(sOrs[i].dom)
    }

  }

  draw(tu)

  return {
    dom: dom,
    redraw: (t) => {
      dom.innerHTML = ''
      draw(t)
    },
    askBasic: (r,q) => { existingQuestion(sReady[actKey(r)],q) },
    askIf: (n,q) => { existingQuestion(sIfs[n].left,q) },
    askOrLeft: (n,q) => { existingQuestion(sOrs[n].left,q) },
    askOrRight: (n,q) => { existingQuestion(sOrs[n].right,q) }
  }
}
