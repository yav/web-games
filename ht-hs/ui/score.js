function drawScore(ps,score) {
  const dom = document.createElement('div')
  dom.classList.add('score')
  dom.classList.add('log-turn')

  const tab = document.createElement('table')

  {
    const tr = document.createElement('tr')
    tr.classList.add('heading')
    const td = document.createElement('td')
    td.textContent = 'Player'
    tr.appendChild(td)
    for (let i = 0; i < ps.length; ++i) {
      const td = document.createElement('td')
      td.textContent = ps[i]
      tr.appendChild(td)
    }
    tab.appendChild(tr)
  }

  const total = {}
  for (stat in score) {
    const entries = score[stat]
    const tr = document.createElement('tr')
    const td = document.createElement('td')
    td.textContent = stat
    tr.appendChild(td)

    for (let i = 0; i < ps.length; ++i) {
      const player = ps[i]
      const pts = entries[player]
      if (total[player] == undefined) total[player] = 0
      total[player] = total[player] + pts
      const td = document.createElement('td')
      td.textContent = pts
      tr.appendChild(td)
    }
    tab.appendChild(tr)
  }

  {
    const tot = document.createElement('tr')
    tot.classList.add('total')
    const td = document.createElement('td')
    td.textContent = 'Total:'
    tot.appendChild(td)
    for (let i = 0; i < ps.length; ++i) {
      const td = document.createElement('td')
      td.textContent = total[ps[i]]
      tot.appendChild(td)
    }
    tab.appendChild(tot)
  }
  dom.appendChild(tab)

  return dom
}
