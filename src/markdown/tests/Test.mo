import MarkdownParser "../MarkdownParser";
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
    "Header 1",
    [
      it(
        "Should convert a header 1",
        do {
          let input = "# Header 1";
          let expected = "<h1>Header 1</h1>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := result;
            };
            case (#err err) {
              Debug.print("Error parsing Header 1");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          assetEqualOrError(actual, expected);
        },
      ),
    ],
  ),
  describe(
    "Paragraph",
    [
      it(
        "Should convert a paragraph",
        do {
          let input = "This is a paragraph";
          let expected = "<p>This is a paragraph</p>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := result;
            };
            case (#err err) {
              Debug.print("Error parsing Paragraph");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          let result = actual == expected;
          assetEqualOrError(actual, expected);
        },
      ),
      it(
        "Should convert a paragraph with multiple lines",
        do {
          let input = "This is a paragraph\nwith multiple lines";
          let expected = "<p>This is a paragraph\nwith multiple lines</p>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Paragraph with multiple lines");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          let result = actual == expected;
          assetEqualOrError(actual, expected);
        },
      ),
      it(
        "Should convert a paragraph with multiple lines and a blank line",
        do {
          let input = "This is a paragraph\nwith multiple lines\n\nand a blank line";
          let expected = "<p>This is a paragraph\nwith multiple lines</p>\n<p>and a blank line</p>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Paragraph with multiple lines and a blank line");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          let result = actual == expected;
          assetEqualOrError(actual, expected);
        },

      ),
    ],
  ),
  describe(
    "Link",
    [
      it(
        "Should convert a link",
        do {
          let input = "[Link](https://www.google.com)";
          let expected = "<p><a href=\"https://www.google.com\">Link</a></p>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := result;
            };
            case (#err err) {
              Debug.print("Error parsing Link");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          let result = actual == expected;
          Debug.print("Result: " # actual);
          assetEqualOrError(actual, expected);
        },
      ),
      it(
        "should convert a link mixed with other text",
        do {
          let input = "This is a [Link](https://www.google.com)";
          let expected = "<p>This is a <a href=\"https://www.google.com\">Link</a></p>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := result;
            };
            case (#err err) {
              Debug.print("Error parsing Link mixed with other text");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          let result = actual == expected;
          Debug.print("Result: " # actual);
          assetEqualOrError(actual, expected);
        },
      ),
      it(
        "should convert a link with text before and after",
        do {
          let input = "This is a [Link](https://www.google.com) with text before and after";
          let expected = "<p>This is a <a href=\"https://www.google.com\">Link</a> with text before and after</p>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := result;
            };
            case (#err err) {
              Debug.print("Error parsing Link with text before and after");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          let result = actual == expected;
          Debug.print("Result: " # actual);
          assetEqualOrError(actual, expected);
        },
      ),
      it("should handle multiple links",
        do {
          let input = "This is a [Link](https://www.google.com) with text before and after and another [Link](https://www.google.com)";
          let expected = "<p>This is a <a href=\"https://www.google.com\">Link</a> with text before and after and another <a href=\"https://www.google.com\">Link</a></p>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := result;
            };
            case (#err err) {
              Debug.print("Error parsing Link with text before and after");
              Debug.print("Expected " # expected # " but got Error: " # err.message);
            };
          };
          let result = actual == expected;
          Debug.print("Result: " # actual);
          assetEqualOrError(actual, expected);
        },
      ),
    ],
  ),
]);
