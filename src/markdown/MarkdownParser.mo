import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Result "mo:base/Result";
import Debug "mo:base/Debug";

module {

  let spacePredicate = #predicate(func(c : Char) : Bool { c == ' ' });
  let newLinePredicate = #predicate(func(c : Char) : Bool { c == '\n' });

  public type ParseError = {
    message : Text;
  };

  public type ParseResult = Result.Result<Text, ParseError>;

  public func parse(md : Text) : ParseResult {
    // Split the text into lines
    let lines = Text.split(md, #predicate(func(c) { c == '\n' }));

    // Parse each line
    let results = Iter.map(lines, parseLine);

    // join the results
    var output = "";
    for (result in results) {
      switch result {
        case (#ok text) {
          output := output # text;
        };
        case (#err error) { return #err(error) };
      };
    };

    if (output == "") {
      return #err({ message = "No output" });
    };

    return #ok(output);

    // Base case
    #err({ message = "Not implemented" });
  };

  func parseLine(line : Text) : ParseResult {
    Debug.print("Line: " # line);
    // Check if the line starts with a hash symbol
    if (Text.startsWith(line, #char '#')) {
      // Extract the text following the hash symbol
      let text = Text.trimStart(line, #char '#');

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(text, spacePredicate);

      // Return the header in HTML format
      return #ok("<h1>" # trimmed # "</h1>");
    };

    // Base case
    return #ok line;
  };
};

// let tokens = Text.tokens(
//   line,
//   #predicate(func(c) { c == ' ' }),
//   // #predicate(func(c) { c == ' ' or c == '\n' }),
// );
// for (token in tokens) {
//   Debug.print("Token: " # token);
// };
