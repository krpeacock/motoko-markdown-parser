import MarkdownParser "../MarkdownParser";
import Debug "mo:base/Debug";

import ActorSpec "./ActorSpec";
type Group = ActorSpec.Group;

let assertTrue = ActorSpec.assertTrue;
let describe = ActorSpec.describe;
let it = ActorSpec.it;
let skip = ActorSpec.skip;
let pending = ActorSpec.pending;
let run = ActorSpec.run;

let success = run([
  describe(
    "Header 1",
    [
      it(
        "Should convert a header 1",
        do {
          let input = "# Header 1\nand more";
          let expected = "<h1>Header 1</h1>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Header 1");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          Debug.print(debug_show actual);
          assertTrue(actual == expected);
        },
      ),
    ],
  ),
]);
