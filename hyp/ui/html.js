function div(classes) {
  const cs = classes.split(' ')
  const dom = document.createElement('div')
  for (let i = 0; i < cs.length; ++i) {
    dom.classList.add(cs[i])
  }
  return dom
}

function img(src) {
  const dom = document.createElement('img')
  dom.setAttribute('src',src)
  return dom
}

function setSize(el,d,x) { el.style[d] = toSize(x) }
function setDim(el,x,y) {
  setSize(el,'width',x)
  setSize(el,'height',y)
}
