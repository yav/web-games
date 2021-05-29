let sendJSON = null
let playerId = null
let iconSize = 26
let gui      = null

const main = () => {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
  if (conn.size) iconSize = conn.size
}

const uiRedraw = (state) => {
  const body = document.getElementById('main')

  const w = 100
  const h = 150

  let s = spriteSheet('img/3.jpg',3800,4500,6,8,30,30)
  for (let r = 0; r < 6; ++r) {
    for (let c = 0; c < 8; ++c) {
      const it = s(c,r,w,h)
      it.classList.add('big')
      it.style.display = 'inline-block'
      it.style.margin = '0.2em'
      body.appendChild(it)
    }
    body.appendChild(html.br())
  }

  s = spriteSheet('img/5.jpg',2400,3750,5,5,30,30)
  for (let r = 0; r < 5; ++r) {
    for (let c = 0; c < 5; ++c) {
      const it = s(c,r,w,h)
      it.classList.add('big')
      it.style.display = 'inline-block'
      it.style.margin = '0.2em'
      body.appendChild(it)
    }
    body.appendChild(html.br())
  }

  s = spriteSheet('img/8.jpg',3360,3750,5,7,30,30)
  for (let r = 0; r < 5; ++r) {
    for (let c = 0; c < 7; ++c) {
      const it = s(c,r,w,h)
      it.classList.add('big')
      it.style.display = 'inline-block'
      it.style.margin = '0.2em'
      body.appendChild(it)
    }
    body.appendChild(html.br())
  }

  s = spriteSheet('img/expedition.jpg',2880,3000,4,6,30,30)
  for (let r = 0; r < 4; ++r) {
    for (let c = 0; c < 6; ++c) {
      const it = s(c,r,w,h)
      it.classList.add('big')
      it.style.display = 'inline-block'
      it.style.margin = '0.2em'
      body.appendChild(it)
    }
    body.appendChild(html.br())
  }







  uiQuestions(state.questions)
}

const uiQuestion = (q) => hsInput({
  })(q.chChoice)


const uiUpdate = hsUpdate({
  })

const spriteSheet = (img,imgW,imgH,rows,cols,cx,cy) => {
  const x = cx
  const y = cy
  const dx = imgW / cols
  const dy = imgH / rows
  const w = dx - 2 * cx
  const h = dy - 2 * cy
  return (a,b,aw,ah) => {
    const dom = html.div('')
    const hr = aw / w
    const vr = ah / h
    dom.style.width = aw + 'px'
    dom.style.height = ah + 'px'
    dom.style.backgroundImage = 'url("' + img + '")'
    dom.style.backgroundPositionX = hr * (-x - a * dx) + 'px'
    dom.style.backgroundPositionY = vr * (-y - b * dy) + 'px'
    dom.style.backgroundSize = (imgW * hr) + 'px ' + (imgH * vr) + 'px'
    return dom
  }
}
