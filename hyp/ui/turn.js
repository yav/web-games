
const uiTurn = (tu) => {
  const dom = div('turn')

  const draw = (t) => {
    const ready = t._turnReady
    for (let i = 0; i < ready.length; ++i) {
      const an = ready[i]
      const el = uiBasicAction({tag:'Times',contents:ready[i]})
      dom.appendChild(el)
    }
  }

  draw(tu)

  return {
    dom: dom,
    redraw: (t) => {
      dom.innerHTML = ''
      draw(t)
    }
  }
}
