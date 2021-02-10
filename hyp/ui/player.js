
const uiPlayer = (p, s) => {
  const dom = div('player')
  dom.appendChild(uiPlayerActions(s._playerBoard))
  return dom
}

const uiPlayerActions = (a) => {
  const dom = div('player-actions')
  for (g in a) {
    dom.appendChild(uiPlayerBoardGroup(a,a[g]))
  }
  return dom
}

const uiPlayerBoardGroup = (name,group) => {
  const dom = div('action-group')
  for (let i = 0; i < group.length; ++i) {
    const a = group[i]
    const hack = { tag: 'OneTime', contents: a.baBenefit }
    dom.appendChild(uiActivation(a.baCost,hack))
  }
  return dom
}

