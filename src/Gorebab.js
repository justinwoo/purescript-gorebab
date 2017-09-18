var RO = require("redux-observable");

exports._of = function (actions) {
  return RO.ActionsObservable.of.apply(null, actions);
};

exports._from = function (actions, scheduler) {
  return RO.ActionsObservable.from.apply(null, actions.concat(scheduler));
};

exports._ofType = function (keys, observable) {
  return observable.ofType.apply(null, keys);
};

exports._combineEpics = RO.combineEpics;

exports._createEpicMiddleware = RO.createEpicMiddleware;
