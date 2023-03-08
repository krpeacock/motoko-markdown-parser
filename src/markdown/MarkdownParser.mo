import Text "./Text";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
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
    #Collection : [StaticPhrasingContent];
    #Image : {
      alt : Text;
      src : Text;
      title : ?Text;
    };
    #Link : {
      href : Text;
      title : ?Text;
      children : [StaticPhrasingContent];
    };
  };

  let spacePattern = #char ' ';
  let newlinePattern = #text "\n";
  let header1Pattern = #text "# ";
  let header2Pattern = #text "## ";
  let header3Pattern = #text "### ";
  let header4Pattern = #text "#### ";
  let header5Pattern = #text "##### ";
  let header6Pattern = #text "###### ";

  let linkPattern = #text "[";
  let linkPatternEnd = #text "](";
  let linkPatternEnd2 = #text ")";

  let imagePattern = #text "![";
  let imagePatternEnd = #text "](";
  let imagePatternEnd2 = #text ")";

  let strongPattern = #text "**";
  let strongPatternEnd = #text "**";

  let emPattern = #text "*";
  let emPatternEnd = #text "*";

  let italicPattern = #text "_";
  let italicPatternEnd = #text "_";

  let codePattern = #text "`";
  let codePatternEnd = #text "`";

  let codeBlockPattern = #text "```";
  let codeBlockPatternEnd = #text "```";

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
    if (Text.startsWith(line, header1Pattern)) {
      // Extract the text following the hash symbol
      let text = Text.trimStart(line, header1Pattern);

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(text, spacePattern);
      let content = parseInlineElements(trimmed);

      // Return the header in HTML format
      return #ok({
        kind = "header";
        depth = ?1;
        children = null;
        content = ?content;

      });
    };

    if (Text.startsWith(line, header2Pattern)) {
      // Extract the text following the hash symbol
      let text = Text.trimStart(line, header2Pattern);

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(text, spacePattern);

      // Return the header in HTML format
      return #ok({
        kind = "header";
        depth = ?2;
        children = null;
        content = ?#Text trimmed;

      });
    };

    if (Text.startsWith(line, header3Pattern)) {
      // Extract the text following the hash symbol
      let text = Text.trimStart(line, header3Pattern);

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(text, spacePattern);

      // Return the header in HTML format
      return #ok({
        kind = "header";
        depth = ?3;
        children = null;
        content = ?#Text trimmed;

      });
    };

    if (Text.startsWith(line, header4Pattern)) {
      // Extract the text following the hash symbol
      let text = Text.trimStart(line, header4Pattern);

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(text, spacePattern);

      // Return the header in HTML format
      return #ok({
        kind = "header";
        depth = ?4;
        children = null;
        content = ?#Text trimmed;

      });
    };

    if (Text.startsWith(line, header5Pattern)) {
      // Extract the text following the hash symbol
      let text = Text.trimStart(line, header5Pattern);

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(text, spacePattern);

      // Return the header in HTML format
      return #ok({
        kind = "header";
        depth = ?5;
        children = null;
        content = ?#Text trimmed;

      });
    };

    if (Text.startsWith(line, header6Pattern)) {
      // Extract the text following the hash symbol
      let text = Text.trimStart(line, header6Pattern);

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(text, spacePattern);

      // Return the header in HTML format
      return #ok({
        kind = "header";
        depth = ?6;
        children = null;
        content = ?#Text trimmed;

      });
    };

    // Base case

    let content = parseInlineElements(line);
    return #ok {
      kind = "paragraph";
      content = ?content;
      depth = null;
      children = null;
    };
  };

  func parseInlineElements(line : Text) : StaticPhrasingContent {
    let elements = Buffer.fromArray<StaticPhrasingContent>([]);

    // A Line may contain multiple inline elements
    // Known elements are links, images, bold, italic, and inline code
    // We will parse each element and return a list of AST nodes

    // Check if the line contains a link
    if (Text.contains(line, linkPattern)) {
      // The link text contains all the text between the square brackets
      elements.add(processLink(line));
    };

    if (elements.size() == 0) {

      return #Text line;

    } else {
      let collection = Buffer.toArray(elements);
      return #Collection collection;

    };

  };

  func processLink(line : Text) : StaticPhrasingContent {
    let textArray = Iter.toArray(Text.toIter(line));
    let size = line.size();
    let sizeLessOne : Nat = size - 1;

    var linkStartIndex = 0;
    var foundStartIndex = false;
    var linkContentsEndIndex = sizeLessOne;
    var foundContentsEndIndex = false;
    var linkUrlStartIndex = sizeLessOne;
    var foundUrlStartIndex = false;
    var linkEndIndex = sizeLessOne;
    var foundEndIndex = false;

    var locatingIndex = true;
    var count = 0;
    while (locatingIndex) {

      Debug.print("linkStartIndex: " # debug_show linkStartIndex);
      Debug.print("linkContentsEndIndex: " # debug_show linkContentsEndIndex);
      Debug.print("linkUrlStartIndex: " # debug_show linkUrlStartIndex);
      Debug.print("linkEndIndex: " # debug_show linkEndIndex);

      if (textArray[linkStartIndex] == '[') {
        foundStartIndex := true;
      };
      if (not foundStartIndex) {
        linkStartIndex := linkStartIndex + 1;
      };

      if (textArray[linkContentsEndIndex] == ']') {
        foundContentsEndIndex := true;
      };
      if (not foundContentsEndIndex) {
        linkContentsEndIndex := linkContentsEndIndex - 1;
      };

      if (textArray[linkEndIndex] == ')') {
        foundEndIndex := true;
      };
      if (not foundEndIndex) {
        linkEndIndex := linkEndIndex - 1;
      };

      if (textArray[linkUrlStartIndex] == '(') {
        foundUrlStartIndex := true;
      };
      if (not foundUrlStartIndex) {
        linkUrlStartIndex := linkUrlStartIndex - 1;
      };

      if (foundUrlStartIndex and foundContentsEndIndex and foundStartIndex and foundEndIndex) {
        locatingIndex := false;
      };

      count := count + 1;
      Debug.print("Count: " # debug_show count);
      if (count >= line.size()) {
        Debug.print("Error: Link parsing failed");
        return #Text line;
      };

    };

    Debug.print("Link start index: " # debug_show linkStartIndex);
    Debug.print("Link contents end index: " # debug_show linkContentsEndIndex);
    Debug.print("Link url start index: " # debug_show linkUrlStartIndex);
    Debug.print("Link end index: " # debug_show linkEndIndex);

    var fullLinkText = "";

    for (i in Iter.range(linkStartIndex, linkEndIndex)) {
      fullLinkText := fullLinkText # Text.fromChar(textArray[i]);
    };

    var linkContents = "";
    var linkUrl = "";

    for (i in Iter.range(linkStartIndex + 1, linkContentsEndIndex - 1)) {
      linkContents := linkContents # Text.fromChar(textArray[i]);
    };

    for (i in Iter.range(linkUrlStartIndex + 1, linkEndIndex - 1)) {
      linkUrl := linkUrl # Text.fromChar(textArray[i]);
    };

    Debug.print("Link text: " # debug_show fullLinkText);
    Debug.print("Link contents: " # debug_show linkContents);
    Debug.print("Link url: " # debug_show linkUrl);

    return #Link {
      kind = "link";
      href = linkUrl;
      title = null;
      children = [#Text linkContents];
    };
  
};

public func renderASTToHTML(ast : [AstNode]) : Text {
  var output = "";

  for (node in Iter.fromArray(ast)) {
    output := output # renderNodeToHTML(node);
  };

  // strip final newline
  output := Text.trimEnd(output, newlinePattern);

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
        case (?#Collection c) {
          content := renderStaticPhrasingContentToHtml(c);
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
        case (?#Collection c) {
          output := "<p>" # renderStaticPhrasingContentToHtml(c) # "</p>";
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

func renderStaticPhrasingContentToHtml(content : [StaticPhrasingContent]) : Text {
  var output = "";

  for (node in Iter.fromArray(content)) {
    switch (node) {
      case (#Text text) {
        output := output # text;
      };
      case (#Link link) {
        output := output # "<a href=\"" # link.href # "\">" # renderStaticPhrasingContentToHtml(link.children) # "</a>";
      };
      case (#Image image) {
        output := output # "<img src=\"" # image.src # "\" alt=\"" # image.alt # "\">";
      };
      case (_) { output := output # "" };
    };
  };

  if (output == "") {
    return "Rendered HTML Error";
  };

  output;
};

};
