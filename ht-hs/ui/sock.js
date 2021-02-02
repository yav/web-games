const handlers = {
    // Edges
    PlaceWorkerOnEdge:    function(...as) { gui.board.placeWorkerOnEdge(...as) }
  , RemoveWorkerFromEdge: function(...as) { gui.board.removeWorkerFromEdge(...as)
    }
  , EdgeRemoveBonus:      function(...as) { gui.board.removeBonus(...as) }
  , EdgeSetBonus:         function(...as) { gui.board.placeBonus(...as) }

    // Nodes
  , PlaceWorkerInOffice:  function(...as) { gui.board.placeWorkerInOffice(...as) }
  , PlaceWorkerInAnnex:   function(...as) { gui.board.placeWorkerInAnnex(...as) }
  , SwapWorkers:          function(...as) { gui.board.swapWorkers(...as) }
  , SetEndVPAt:           function(...as) { gui.board.placeWorkerOnVP(...as) }

    // misc
  , SetFull: function(...as) { gui.board.setFull(...as) }

    // player
  , SetWorkerPreference: function (w) {
      gui.playerUI(w.owner).setPreference(w.shape)
    }
  , ChangeAvailble: function(w,n) {
      gui.playerUI(w.owner).changeWorkers('available',w.shape,n)
    }
  , ChangeUnavailable: function(w,n) {
      gui.playerUI(w.owner).changeWorkers('unavailable',w.shape,n)
    }
  , UseGateway: function(g) {}
  , ChangeVP: function(player,n) { gui.playerUI(player).changeVP(n) }
  , Upgrade: function(player,stat) { gui.playerUI(player).upgrade(stat) }
  , GainBonusToken: function(player,token) { gui.playerUI(player).addBonus(token) }
  , UseBonusToken: function(player,bonus) {
      const ui = gui.playerUI(player)
      ui.removeBonus(bonus)
      ui.addSpentBonus()
    }


    // turn
  , NewTurn: function(t) { gui.turn.remove(); gui.turn = drawTurn(t) }
  , ChangeDoneActions: function(...as) { gui.turn.changeDone(...as) }
  , ChangeActionLimit: function(...as) { gui.turn.changeLimit(...as) }
  , AddWorkerToHand:   function(...as) { gui.turn.addWorkerToHand(...as) }
  , RemoveWorkerFromHand: function(...as) { gui.turn.removeWorkerFromHand(...as) }
  , DrawBonusToken: function() { gui.changeTokenCount(-1) }
  , PlacingBonus: function(...as) { gui.setPlacing(...as) }
  , AchieveBonusRoute: function() {}

  // log
  , Log: function(...as) { gui.log.addLog(...as) }

  , EndGame: function() { gui.reload() }
  }

function sendJSON(ws,obj) {
  ws.send(JSON.stringify(obj))
}

function srvConnect() {
  const obj = new URL(window.location)
  const info = obj.searchParams
  const url = 'ws://' + obj.host + '/ws'
  const s = Number(info.get('size'))
  if (s) { boardSize = s }
  console.log("Connecting to: " + url)
  const ws = new WebSocket(url)

  const handler = hsOutMsg(
    { CurGameState: function(state) { uiRedraw(ws,state) }
    , AskQuestions: uiQuestions
    , GameUpdate: hsGameUpdate(handlers)
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



