
const uiTurn = (t) => {
  const dom = div('turn')
  const ready = t._turnReady
  for (let i = 0; i < ready.length; ++i) {
    const an = ready[i]
    const el = uiBasicAction({tag:'Times',contents:ready[i]})
    dom.appendChild(el)
  }
  return {
    dom: dom,
    askButton: (txt,q) => {
      const btn = div('button')
      btn.textContent = txt
      newQuestion(btn,q)
      dom.appendChild(btn)
    }
  }
}
