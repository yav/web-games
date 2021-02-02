function sendJSON(ws,obj) {
  ws.send(JSON.stringify(obj))
}

function srvConnect() {
  const obj = new URL(window.location)
  const info = obj.searchParams
  const url = 'ws://' + obj.host + '/ws'
  console.log("Connecting to: " + url)
  const ws = new WebSocket(url)

  const handler = hsOutMsg(
    { CurGameState: uiRedraw
    , AskQuestions: uiQuestions
    , GameUpdate:   uiUpdate
    })

  ws.onopen = function(e) {
    console.log('Connected.')
    playerId = info.get('player') // stored in global
    console.log("We are player: " + playerId)
    ws.send(playerId)
    sendJSON(ws,{ tag: 'reload' })
  }

  ws.onmessage = function(e) {
    const msg = JSON.parse(e.data)
    console.log('Received:')
    console.log(msg)
    handler(msg)
  }

  ws.onclose = function(e) {
    console.log('Disconnected.')
  }

  ws.onerror = function(e) {
    console.log('error')
    console.log(e)
  }
}



