
const uiLog = (evs) => {
  const dom = div('log')
  setSize(dom, 'width', 16.5 * iconSize)

  let container = dom

  const addEv = (ev) => {
    const ent = div('log-entry')
    for (let i = 0; i < ev.length; ++i) {
      const it = ev[i]
      switch (it.tag) {
        case 'LogNewTurn':
          container = div('log-box')
          dom.prepend(container)
          if (it.contents)
            container.appendChild(uiPlayerBadge(it.contents))
          break

        case 'LogText':
          ent.appendChild(span(it.contents))
          break

        case 'LogCube':
          ent.appendChild(uiCube(it.contents))
          break

        case 'LogUpgrade':
          ent.appendChild(span(it.contents[1]))
          ent.appendChild(uiUpgradeIcon(it.contents[0]))
          break

        case 'LogPlayer':
          ent.appendChild(uiPlayerBadge(it.contents))
          break

        case 'LogAchievement':
          ent.appendChild(uiAchievement(it.contents))
          break

        case 'LogHex': {
          const el = span('here')
          el.classList.add('log-hover')
          el.addEventListener('mouseenter',() => gui.board.hi(it.contents))
          el.addEventListener('mouseleave',() => gui.board.lo(it.contents))
          ent.appendChild(el)
          break
        }

        case 'LogCity': {
          const el = span('city')
          el.classList.add('log-hover')
          const loc = it.contents[0]
          const id  = it.contents[1]
          el.addEventListener('mouseenter',() => gui.board.hiCity(loc,id))
          el.addEventListener('mouseleave',() => gui.board.loCity(loc,id))
          ent.appendChild(el)
          break
        }
        case 'LogRuin': {
          const el = span('ruin')
          el.classList.add('log-hover')
          const loc = it.contents[0]
          const id  = it.contents[1]
          el.addEventListener('mouseenter',() => gui.board.hiRuin(loc,id))
          el.addEventListener('mouseleave',() => gui.board.loRuin(loc,id))
          ent.appendChild(el)
          break
        }

        default:
          ent.appendChild(span(JSON.stringify(it)))
      }
      ent.appendChild(span(' '))
    }
    container.appendChild(ent)
  }

  for (let i = evs.length - 1; i >= 0; --i) {
    addEv(evs[i])
  }

  return {
    dom: dom,
    logEvent: addEv
  }
}



