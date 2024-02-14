export
type Question<Q> = {
  chChoice: Q,
  chHelp: string,
  chStateName: number
}

export
type CallBacks<S,U,Q> = {
  uiRedraw: (s: S) => void,
  uiSetQuestion: (q: string) => void,
  uiQuestion: (q: Question<Q>) => void,
  uiUpdate: (u: U) => void
}

export
type Connection<Q> = {
  playerId: null | string,
  sendJSON: (xs: object) => void,
  uiQuestions: (qs: [string,Question<Q>[]]) => void
}

export
function srvConnect<S,U,Q>(fun: CallBacks<S,U,Q>): Connection<Q> {
    const obj = new URL(window.location.href)
    const info = obj.searchParams
    const url = 'ws://' + obj.host + '/ws'
    console.log("Connecting to: " + url)
    const ws = new WebSocket(url)

    const playerId = info.get('player')
    function sendJSON(obj: object) { ws.send(JSON.stringify(obj)) }

    ws.onopen = (e) => {
      console.log('Connected.')
      console.log('We are player: ' + playerId)
      ws.send(playerId ? playerId : "guest")
      sendJSON({ tag: 'reload' })
    }

    ws.onmessage = (e) => {
      const msg = JSON.parse(e.data)
      console.log('Received:')
      console.log(msg)
      switch (msg.tag) {
        case "CurGameState":  return fun.uiRedraw(msg.contents)
        case "SetQuestion":   return fun.uiSetQuestion(msg.contents)
        case "AskQuestions":  return uiQuestions(msg.contents)
        case "GameUpdate":    return fun.uiUpdate(msg.contents)
      }
    }

    ws.onclose = (e) => {
      console.log('Disconnected.')
    }

    ws.onerror = (e) => {
      console.log('error')
      console.log(e)
    }

    function uiQuestions(qs0: [string,Question<Q>[]]) {
        const [q,qs] = qs0
        fun.uiSetQuestion(q)
        for (let i = 0; i < qs.length; ++i) {
          fun.uiQuestion(qs[i])
        }
    }

    return {
        playerId: playerId,
        sendJSON: sendJSON,
        uiQuestions: uiQuestions
    }
}
