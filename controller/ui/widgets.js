
// Settings for a game
const settings = (name,comp,ps,opts) => {

  const outer = document.createElement('div')
  outer.style.display = 'flex'
  outer.style.display = 'flex'
  outer.style.flexDirection = 'column'
  outer.style.justifyContent = 'center'
  outer.style.alignItems     = 'center'

  const dom = document.createElement('div')
  outer.appendChild(dom)
  dom.style.display = 'flex'
  dom.style.flexWrap = 'wrap'
  dom.style.alignItems = 'flex-start'
  dom.style.alignContent = 'center'

  const p = players(ps)
  dom.appendChild(p.dom)

  const sets = []
  for (let i = 0; i < opts.length; ++i) {
    const os = opts[i]
    sets[i] = choices(os[0],os[1],os[2])
    dom.appendChild(sets[i].dom)
  }

  const btn = document.createElement('div')
  outer.appendChild(btn)
  btn.textContent = 'Start ' + name
  btn.style.display = 'inline-block'
  btn.style.backgroundColor = 'orange'
  btn.style.padding = '0.5em'
  btn.style.borderRadius = '0.2em'
  btn.style.border = '0.2em solid black'
  btn.style.fontWeight = 'bold'
  btn.style.cursor = 'pointer'
  btn.addEventListener('click', () => {
    const url = new URL('/new/' + comp, document.URL)
    const params = url.searchParams
    const ps = p.getPlayers()
    for (let i = 0; i < ps.length; ++i) {
      params.append('player',ps[i])
    }

    for (let i = 0; i < opts.length; ++i) {
      params.append(opts[i][0], sets[i].getChoice())
    }
    window.location.replace(url)
  })

  return outer
}

const labelBox = (x) => {
  const dom = document.createElement('div')
  dom.style.backgroundColor = 'white'
  dom.style.display = 'inline-block'
  dom.style.position = 'relative'
  dom.style.border   = '0.1em #ccc solid'
  dom.style.borderRadius = '0.5em'
  dom.style.margin   = '0.5em'
  dom.style.padding = '0.2em'

  const lab = document.createElement('div')
  lab.style.backgroundColor = 'white'
  lab.style.paddingLeft = '0.2em'
  lab.style.paddingRight = '0.2em'
  lab.style.marginBottom = '0.5em'
  lab.style.borderRadius = '0.5em'
  dom.appendChild(lab)
  lab.textContent = x
  return dom
}

const players = (cs) => {
  const boxes = []
  const dom = document.createElement('table')
  for (let i = 0; i < cs.length; ++i) {
    const row = document.createElement('tr')
    dom.appendChild(row)

    const td1 = document.createElement('td')
    row.appendChild(td1)
    const col = document.createElement('div')
    td1.appendChild(col)
    col.style.width = '1em'
    col.style.height = '1em'
    col.style.backgroundColor = cs[i]
    col.style.borderRadius = '0.2em'
    col.style.border       = '0.2em solid black'

    const td2 = document.createElement('td')
    row.appendChild(td2)
    const box = document.createElement('input')
    td2.appendChild(box)
    boxes[i] = box
  }

  const lab = labelBox('Players')
  lab.appendChild(dom)
  return {
    dom: lab,

    getPlayers: () => {
      const ps = []
      let next = 0
      for (let i = 0; i < boxes.length; ++i) {
        const val = boxes[i].value.trim()
        if (val.length > 0) {
          ps[next++] = val + ':' + cs[i]    // XXX: filter :?
        }
      }
      return ps
    }

  }
}

const choices = (q,cs,def) => {
  const dom = labelBox(q)
  const bs = {}
  for (let i = 0; i < cs.length; ++i) {
    const entry = document.createElement('div')
    const it = document.createElement('input')
    it.setAttribute('type','radio')
    it.setAttribute('name',q)
    it.setAttribute('value',cs[i])
    it.setAttribute('id',cs[i])
    if (cs[i] === def) it.setAttribute('checked',true)

    const lab = document.createElement('label')
    lab.setAttribute('for',cs[i])
    lab.textContent = cs[i]

    entry.appendChild(it)
    entry.appendChild(lab)
    dom.appendChild(entry)
  }

  return {
    dom: dom,
    getChoice: () =>
      document.querySelector('input[name="' + q + '"]:checked').value
  }
}


