function div(classes) {
  const cs = classes.split(' ')
  const dom = document.createElement('div')
  for (let i = 0; i < cs.length; ++i) {
    if(cs[i] != '') dom.classList.add(cs[i])
  }
  return dom
}

function span(x) {
  const dom = document.createElement('span')
  dom.textContent = x
  return dom
}

function img(src) {
  const dom = document.createElement('img')
  dom.setAttribute('src',src)
  return dom
}

function svgObj(src) {
  const dom = document.createElement('object')
  dom.setAttribute('type','image/svg+xml')
  dom.setAttribute('data',src)
  return dom
}

function svg(src) {
  const nsS = 'http://www.w3.org/2000/svg'
  const nsL = 'http://www.w3.org/1999/xlink'
  const svg = document.createElementNS(nsS, 'svg')
  const use = document.createElementNS(nsS,'use')
  use.setAttribute('href',src)
  svg.appendChild(use)
  return svg
}



function setSize(el,d,x) { el.style[d] = toSize(x) }
function setDim(el,x,y) {
  setSize(el,'width',x)
  setSize(el,'height',y)
}
