import Utils "../Utils";
import Debug "mo:base/Debug";
import Text "mo:base/Text";

import ActorSpec "./ActorSpec";
type Group = ActorSpec.Group;

let assertTrue = ActorSpec.assertTrue;
let describe = ActorSpec.describe;
let it = ActorSpec.it;
let skip = ActorSpec.skip;
let pending = ActorSpec.pending;
let run = ActorSpec.run;

func assetEqualOrError(actual : Text, expected : Text) : Bool {
  if (actual != expected) {
    Debug.print("Expected " # expected # " but got " # actual);
  };
  return Text.equal(actual, expected);
};

let success = run([
  describe(
    "substring",
    [
      it(
        "should return the substring",
        do {

          Debug.print("Test: " # Utils.textSubstring("Hello, World!", 0, 5));
          assertTrue(

             assetEqualOrError(
              Utils.textSubstring("Hello, World!", 0, 5),
              "Hello"
            ));
          
        }
      ),
    ]
  )]);
