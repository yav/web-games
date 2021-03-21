


const uiMarkets = (ms) => {
  const dom = div('markets')
  const decks = {}
  for (deck in ms) {
    const m = uiMarket(deck,ms[deck])
    decks[deck] = m
    dom.appendChild(m.dom)
  }

  return {
    dom: dom,
    setOffers: (d,m) => decks[d].setOffers(m)
  }
}


const uiMarket = (deck,offer) => {
  const dom = div('market')
  let offers
  dom.appendChild(span(deck)) // XXX: colors

  const setOffers = (os) => {
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
    setOffers: setOffers
  }

}


