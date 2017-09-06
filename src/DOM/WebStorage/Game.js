// module DOM.WebStorage.Game

exports.savedGamesChange = function(handler) {
  return function() {
    var oldGames = localStorage.getItem("savedGames");
    handler(oldGames)();

    window.addEventListener("savedGamesChange", function(ev) {
      handler(ev.detail)();
    });
  };
};

exports.saveGameLS = function(g) {
  return function() {
    var oldGames = localStorage.getItem("savedGames");
    if (!oldGames) oldGames = [];
    else oldGames = JSON.parse(oldGames);

    oldGames.push(JSON.parse(g));
    var newGames = JSON.stringify(oldGames);
    localStorage.setItem("savedGames", newGames);

    var event = new CustomEvent("savedGamesChange", {
      detail: newGames
    });

    window.dispatchEvent(event);
  };
};
