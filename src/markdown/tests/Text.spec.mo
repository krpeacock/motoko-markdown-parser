import Utils "../Utils";
import Debug "mo:base/Debug";
import Text "../Text";
import Nat "mo:base/Nat";

import ActorSpec "./ActorSpec";
import Char "mo:base/Char";
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
        "zero length",
        do {
          assetEqualOrError(
            Text.substring("abc", 0, 0),
            "",
          );
        },
      ),
      it(
        "length of 1 from start",
        do {
          assetEqualOrError(
            Text.substring("abc", 0, 1),
            "a",
          );
        },
      ),
      it(
        "length of 2 from start",
        do {
          assetEqualOrError(
            Text.substring("abc", 0, 2),
            "ab",
          );
        },
      ),
      it(
        "length of 3 from start",
        do {
          assetEqualOrError(
            Text.substring("abc", 0, 3),
            "abc",
          );
        },
      ),
      it(
        "length of 1 from middle",
        do {
          assetEqualOrError(
            Text.substring("abc", 1, 1),
            "b",
          );
        },
      ),
      it(
        "length of 2 from middle",
        do {
          assetEqualOrError(
            Text.substring("abc", 1, 2),
            "bc",
          );
        },
      ),
      it(
        "length of 1 from end",
        do {
          assetEqualOrError(
            Text.substring("abc", 2, 1),
            "c",
          );
        },
      ),
      it(
        "length of 2 from end",
        do {
          assetEqualOrError(
            Text.substring("abc", 1, 2),
            "bc",
          );
        },
      ),
      it(
        "should handle start past end",
        do {
          assetEqualOrError(
            Text.substring("abc", 3, 1),
            "",
          );
        },
      ),
    ],
  ),
  describe(
    "indexOf",
    [
      it(
        "should find a character",
        do {
          switch (Text.indexOf("abc", #text "b")) {
            case (null) {
              Debug.print("Expected to find a character");
              false
            };
            case (?index) {
              Nat.equal(index, 1)
            };
          };
        },
      ),
      it(
        "should not find a character",
        do {
          switch (Text.indexOf("abc", #text "d")) {
            case (null) {
              true
            };
            case (?_) {
              Debug.print("Expected not to find a character");
              false
            };
          };
        },
      ),
      it(
        "should find a substring",
        do {
          switch (Text.indexOf("abc", #text "bc")) {
            case (null) {
              Debug.print("Expected to find a substring");
              false
            };
            case (?index) {
              Nat.equal(index, 1)
            };
          };
        },
      ),
      it(
        "should not find a substring",
        do {
          switch (Text.indexOf("abc", #text "bd")) {
            case (null) {
              true
            };
            case (?_) {
              Debug.print("Expected not to find a substring");
              false
            };
          };
        },
      ),
      it(
        "should find a character",
        do {
          switch (Text.indexOf("abc", #char 'b')) {
            case (null) {
              Debug.print("Expected to find a character");
              false
            };
            case (?index) {
              Nat.equal(index, 1)
            };
          };
        },
      )
    ]
  )
]);
