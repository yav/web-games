
const uiTurn = (tu) => {
  const dom = div('turn')

  let basic = {}

  const draw = (t) => {
    basic = {}
    const ready = t._turnReady
    for (let i = 0; i < ready.length; ++i) {
      const an = ready[i]
      const r = an[0].tag
      basic[r] = uiBasicAction({tag:'Times',contents:ready[i]})
      dom.appendChild(basic[r])
    }
  }

  draw(tu)

  return {
    dom: dom,
    redraw: (t) => {
      dom.innerHTML = ''
      draw(t)
    },
    askBasic: (r,q) => { existingQuestion(basic[r.tag],q) }
  }
}
