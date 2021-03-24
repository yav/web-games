


const uiMarkets = (ms) => {
  const dom = div('markets')
  const decks = {}
  for (let i = 1; i <= 4; ++i) {
    const deck = 'Deck' + i
    const m = uiMarket(deck,ms[deck])
    decks[deck] = m
    dom.appendChild(m.dom)
  }

  return {
    dom: dom,
    setOffers: (d,m) => decks[d].setOffers(m),
    askMarketDeck: (d,q) => decks[d].askMarketDeck(q),
    askMarketItem: (d,n,q) => decks[d].askMarketItem(n,q)
  }
}


const uiMarket = (deck,offer) => {
  const dom = div('market')
  let offers = []
  const name = uiMarketName(deck)
  dom.appendChild(name)

  const setOffers = (os) => {
    for (let i = 0; i < offers.length; ++i)
      offers[i].dom.remove()

    offers = []
    for (let i = 0; i < os.length; ++i) {
      const o = uiTech(os[i])
      offers[i] = o
      dom.appendChild(o.dom)
    }
  }

  setOffers(offer)

  return {
    dom: dom,
    setOffers: setOffers,
    askMarketDeck: (q) => existingQuestion(name,q),
    askMarketItem: (n,q) => offers[n].ask(q)
  }

}


const uiMarketName = (deck) => {
  const dom = div('deck-name')

  const w = iconSize/2
  const d1 = div('icon')
  setDim(d1,w,w)
  const d2 = div('icon')
  setDim(d2,w,w)
  switch(deck) {
    case 'Deck1':
      d1.classList.add('red')
      d2.classList.add('green')
      break
    case 'Deck2':
      d1.classList.add('yellow')
      d2.classList.add('purple')
      break
    case 'Deck3':
      d1.classList.add('orange')
      d2.classList.add('blue')
      break
    case 'Deck4':
      d1.classList.add('gray')
      d2.classList.add('wild')
      break
  }
  dom.appendChild(d1)
  dom.appendChild(span(" "))
  dom.appendChild(d2)

  return dom
}

