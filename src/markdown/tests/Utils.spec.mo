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
          assertTrue(
            assetEqualOrError(
              Utils.textSubstring("Hello, World!", 0, 5),
              "Hello",
            ),
          );

        },
      ),
      it(
        "should return the substring from the middle",
        do {
          assertTrue(
            assetEqualOrError(
              Utils.textSubstring("Hello, World!", 7, 5),
              "World",
            ),
          );

        },
      ),
      it(
        "should handle out of bounds",
        do {
          assertTrue(
            assetEqualOrError(
              Utils.textSubstring("Hello, World!", 0, 100),
              "Hello, World!",
            ),
          );
        },
      ),
      it(
        "should handle negative length",
        do {
          Debug.print("Test: " # Utils.textSubstring("Hello, World!", 0, -1));
          assertTrue(

            assetEqualOrError(
              Utils.textSubstring("Hello, World!", 0, -1),
              "",
            ),
          );
        },
      ),
    ],
  ),
  describe(
    "textIndexOf",
    [
      it(
        "should return the index of the first occurrence",
        do {
          assertTrue(
            Utils.textIndexOf("Hello, World!", #text "World") == 7,
          );
        },
      ),
      it(
        "should return -1 if the substring is not found",
        do {
          assertTrue(
            Utils.textIndexOf("Hello, World!", #text "Bird!") == -1,
          );
        },
      ),
      it(
        "should return -1 if the substring is empty",
        do {
          assertTrue(
            Utils.textIndexOf("Hello, World!", #text "") == -1,
          );
        },
      ),
      it(
        "should return -1 if the string is empty",
        do {
          assertTrue(
            Utils.textIndexOf("", #text "World") == -1,
          );
        },
      ),
    ],
  ),
]);
