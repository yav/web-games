const html = (() => {

  let scale = 1
  const setSize = (el,d,x) => { el.style[d] = (x * scale) + 'px' }

  return {

    getBody: () => {
      return document.getElementById('main')
    },

    setScale: (s) => {
      scale = s
    },

    div: (classes) => {
      const cs = classes.split(' ')
      const dom = document.createElement('div')
      for (let i = 0; i < cs.length; ++i) {
        if(cs[i] != '') dom.classList.add(cs[i])
      }
      return dom
    },

    br: () => document.createElement('br'),

    span: (x) => {
      const dom = document.createElement('span')
      dom.textContent = x
      return dom
    },

    img: (src) => {
      const dom = document.createElement('img')
      dom.setAttribute('src',src)
      return dom
    },

    svg: (src) => {
      const nsS = 'http://www.w3.org/2000/svg'
      const nsL = 'http://www.w3.org/1999/xlink'
      const svg = document.createElementNS(nsS, 'svg')
      const use = document.createElementNS(nsS,'use')
      use.setAttribute('href',src)
      svg.appendChild(use)
      return svg
    },

    setSize: setSize,

    setDim: (el,x,y) => {
      setSize(el,'width',x)
      setSize(el,'height',y)
    },

    setLoc: (el,x,y) => {
      setSize(el,'left',x)
      setSize(el,'top',y)
    }

  }})()
