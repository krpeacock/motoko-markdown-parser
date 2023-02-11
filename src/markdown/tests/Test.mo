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

func assetEqualOrError(actual : Text, expected : Text) : Bool {
  if (actual != expected) {
    Debug.print("Expected " # expected # " but got " # actual);
  };
  return actual == expected;
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
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Header 1");
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
    "Header 2",
    [
      it(
        "Should convert a header 2",
        do {
          let input = "## Header 2";
          let expected = "<h2>Header 2</h2>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Header 2");
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
    "Header 3",
    [
      it(
        "Should convert a header 3",
        do {
          let input = "### Header 3";
          let expected = "<h3>Header 3</h3>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Header 3");
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
    "Header 4",
    [
      it(
        "Should convert a header 4",
        do {
          let input = "#### Header 4";
          let expected = "<h4>Header 4</h4>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Header 4");
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
    "Header 5",
    [
      it(
        "Should convert a header 5",
        do {
          let input = "##### Header 5";
          let expected = "<h5>Header 5</h5>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Header 5");
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
    "Header 6",
    [
      it(
        "Should convert a header 6",
        do {
          let input = "###### Header 6";
          let expected = "<h6>Header 6</h6>";
          var actual : Text = "";
          switch (MarkdownParser.parse(input)) {
            case (#ok result) {
              actual := expected;
            };
            case (#err err) {
              Debug.print("Error parsing Header 6");
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
              actual := expected;
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
]);
