Elm.Native = Elm.Native || {};
Elm.Native.Location = {};

Elm.Native.Location.make = function(localRuntime){

  var NS = Elm.Native.Signal.make(localRuntime);
  var node = window;

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Location = localRuntime.Native.Location || {};
  if (localRuntime.Native.Location.values){
    return localRuntime.Native.Location.values;
  }

  var url = NS.input('Location.url', window.location.toString());
  var hash = NS.input('Location.hash', window.location.hash);

  function notify () {
    localRuntime.notify(url.id, window.location.toString());
    localRuntime.notify(hash.id, window.location.hash);
  }

  localRuntime.addListener([url.id, hash.id], node, 'hashchange', notify);
  localRuntime.addListener([url.id, hash.id], node, 'load', notify);

  return {
    hash: hash,
    url: url
  };
}
