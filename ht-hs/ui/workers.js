function drawWorker(size,worker) {
  const dom = document.createElement('div')
  dom.classList.add('worker',worker.shape,gui.colors[worker.owner])
  const ssize = size ? workerSize(size,worker.shape) : '2ex'
  const style = dom.style
  style.width = ssize
  style.height = ssize
  style.position = 'relative'
  return dom
}

function workerSize(size,shape) {
  return (shape == 'Disc') ? size * 1.5 : size
}

function drawWorkerAt(loc,size,worker) {
  const dom = drawWorker(size,worker)
  const style = dom.style
  style.position = 'absolute'
  style.left = loc.x + 'px'
  style.top = loc.y + 'px'
  style.borderWidth = size / 5
  return dom
}





