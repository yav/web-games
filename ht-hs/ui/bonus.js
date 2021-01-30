function drawBonusToken(h,b,r) {
  const dom = document.createElement('div')
  dom.classList.add('bonus')

  const img = document.createElement('img')
  img.setAttribute('src','img/token/' + b + '.png')
  const style = img.style
  style.width = h
  style.height = h
  if (r !== undefined) {
    style.transform = 'rotate(' + r + 'deg)'
  }
  dom.appendChild(img)

  const help = document.createElement('div')
  help.classList.add('help')

  const helpImg = document.createElement('img')
  helpImg.setAttribute('src','img/token/' + b + '.png')
  helpImg.style.width = 2 * h
  helpImg.style.height = 2 * h
  help.appendChild(helpImg)
  const helpText = document.createElement('div')
  helpText.textContent = bonusHelp[b]
  help.appendChild(helpText)
  dom.appendChild(help)

  dom.addEventListener('mouseenter',function() {
    help.style.display = 'inline-block'
    help.style.zIndex = 3
  })
  dom.addEventListener('mouseleave',function() {
    help.style.display = 'none'
    help.style.zIndex = 0
  })
  return dom
}

function drawBonusTokenAt(loc,h,b) {
  const dom = drawBonusToken(h,b,loc.rotate)
  const style = dom.style
  style.position = 'absolute'
  style.left = loc.x + 'px'
  style.top  = loc.y + 'px'
  dom.classList.add('bonus')
  return dom
}

const bonusHelp =
  { BonusUpgrade:
      'Upgrade 1 player skill'

  , BonusSwap:
      'Swap an opponent\'s office with the office before it.'

  , BonusMove:
      'Move 3 opponent workers within the same region, one at a time.'

  , BonusExtra:
      'Build an annex.'

  , BonusAct3:
      'Gain 3 actions.'

  , BonusAct4:
      'Gain 4 actions.'

  , BonusPlace2:
      'Place 2 available workers into provinces.'

  , BonusMove2:
      'Pick up 2 workers, then put them down in the same region.'

  , BonusGainPrivilege:
      'Upgrade priviledge skill'

  , BonusBuildInGreen:
      'Use an available worker to build an office in a green city.'

  , BonusReuse2:
      'Move 2 workers from this route, instead of collecting them.'
  }
