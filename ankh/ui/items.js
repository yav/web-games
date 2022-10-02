const itemWidth = hexWidth * 0.75

function item(name,cl) {
  const dom = html.svg('images/' + name + '.svg#it')
  dom.classList.add(cl)
  dom.setAttribute('viewBox','0 0 512 512')
  html.setDim(dom, itemWidth, itemWidth)
  return dom
}
