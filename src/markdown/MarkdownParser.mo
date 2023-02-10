import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Result "mo:base/Result";
import Debug "mo:base/Debug";
import Buffer "mo:base/Buffer";

module {
  public type AstNode = {
    kind : Text;
    children : ?[AstNode];
    depth : ?Nat;
    content : ?StaticPhrasingContent;
  };

  public type StaticPhrasingContent = {
    #Text : Text;
    #Strong : Text;
    // #Image : Text;
  };

  let spacePredicate = #predicate(func(c : Char) : Bool { c == ' ' });
  let newLinePredicate = #predicate(func(c : Char) : Bool { c == '\n' });

  public type ParseError = {
    message : Text;
  };

  public type ParseResult = Result.Result<Text, ParseError>;
  public type ASTParseResult = Result.Result<AstNode, ParseError>;
  public type ASTListResult = Result.Result<[AstNode], ParseError>;

  public func parse(md : Text) : ParseResult {
    let ast = parseToAST(md);
    switch ast {
      case (#ok ast) {
        let html = renderASTToHTML(ast);
        return #ok(html);
      };
      case (#err error) { return #err(error) };
    };
    // Base case
    #err({ message = "Parse Error" });
  };

  public func parseToAST(md : Text) : ASTListResult {
    // Split the text into lines
    let lines = Text.split(md, #predicate(func(c) { c == '\n' }));

    // Parse each line
    let results = Iter.map(lines, parseLine);

    // join the results
    var output = Buffer.fromArray<AstNode>([]);
    for (result in results) {
      switch result {
        case (#ok ast) {
          output.add(ast);
        };
        case (#err error) { return #err(error) };
      };
    };

    if (output.size() == 0) {
      return #err({ message = "No output" });
    };

    let arr : [AstNode] = Buffer.toArray(output);

    return #ok(arr);

    // Base case
    #err({ message = "Not implemented" });
  };

  func parseLine(line : Text) : ASTParseResult {
    // Check if the line starts with a hash symbol
    if (Text.startsWith(line, #char '#')) {
      // Extract the text following the hash symbol
      let text = Text.trimStart(line, #char '#');

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(text, spacePredicate);

      // Return the header in HTML format
      return #ok({
        kind = "header";
        depth = ?1;
        children = null;
        content = ?#Text trimmed;

      });
    };

    // Base case
    return #ok {
      kind = "paragraph";
      content = ?#Text line;
      depth = null;
      children = null;
    };
  };

  public func renderASTToHTML(ast : [AstNode]) : Text {
    var output = "";

    for (node in Iter.fromArray(ast)) {
      output := output # renderNodeToHTML(node);
    };

    if (output == "") {
      return "Rendered HTML Error";
    };

    output;
  };

  func renderNodeToHTML(node : AstNode) : Text {
    var output = "";
    var content = "";
    switch (node.kind) {
      case "header" {

        switch (node.content) {
          case (?#Text text) {
            content := text;
          };
          case (_) { output := "<h1></h1>" };
        };
        switch (node.depth) {
          case (?1) {
            output := "<h1>" # content # "</h1>";
          };
          case (_) { output := "<h6>" # content # "</h6>" };
        };
      };

      case "paragraph" {
        switch (node.content) {
          case (?#Text text) {
            output := "<p>" # text # "</p>";
          };
          case (_) { output := "<p></p>" };
        };
      };
      case (_) { output := "<p></p>" };
    };

    if (output == "") {
      return "Rendered HTML Error";
    };
    output := output # "\n";
    output;
  };

};
