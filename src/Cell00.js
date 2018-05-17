// writeRef :: forall a. EffFn2 E (Ref a) a Unit
exports.writeRef = function(ref, value) {
  ref.value = value;
};
