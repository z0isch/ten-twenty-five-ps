// module DOM.WebStorage.Game

getSavedGames = function() {
  var oldGames = localStorage.getItem("savedGames");
  if (!oldGames) oldGames = [];
  else oldGames = JSON.parse(oldGames);
  return oldGames;
};

dispatchSavedGamesChange = function(g) {
  var event = new CustomEvent("savedGamesChange", {
    detail: g
  });

  window.dispatchEvent(event);
};

exports.savedGamesChange = function(handler) {
  return function() {
    var games = getSavedGames();
    handler(games)();

    window.addEventListener("savedGamesChange", function(ev) {
      handler(ev.detail)();
    });
  };
};

exports.deleteSavedGames = function() {
  localStorage.removeItem("savedGames");
  dispatchSavedGamesChange([]);
};

exports.saveGame = function(g) {
  return function() {
    games = getSavedGames();

    games.push(g);
    localStorage.setItem("savedGames", JSON.stringify(games));
    dispatchSavedGamesChange(games);
  };
};
