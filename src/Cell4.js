// unsafeNull :: forall a. a
exports.unsafeNull = null;

// unsafeIsNull :: forall a. a -> Boolean
exports.unsafeIsNull = function(x) {
  return x === null;
};
