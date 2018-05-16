// foreach_ :: forall e a. Array a -> EffFn1 e a Unit -> Eff e Unit
exports.foreach_ = function(array) {
  return function(fn) {
    return function() {
      for(var i = 0; i < array.length; i++) {
        fn(array[i]);
      }
    };
  };
};
