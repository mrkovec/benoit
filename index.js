'use strict'

import Elm from './Main'

window.onload = () => {
  let appcontainer = document.getElementById('app')
  let app = Elm.Player.embed(appcontainer)

  // var target = document.getElementById('some-id')

  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      console.log(mutation.type)
    })
  })

  var config = { attributes: true, childList: true, characterData: true }
  observer.observe(appcontainer, config)
}
