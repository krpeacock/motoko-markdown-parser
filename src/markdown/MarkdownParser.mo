import Text "./Text";
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

  func parseInlineElements (line: Text) : StaticPhrasingContent {
    let elements = Buffer.fromArray<StaticPhrasingContent>([]);

    // A Line may contain multiple inline elements
    // Known elements are links, images, bold, italic, and inline code
    // We will parse each element and return a list of AST nodes

    // Check if the line contains a link
    if (Text.contains(line, linkPattern)) {
      // The link text contains all the text between the square brackets
      var linkText = Text.trimStart(line, linkPattern);
      let linkTextEnd = Text.indexOf(linkText,linkPatternEnd);
      switch linkTextEnd {
        case (?end) {
          linkText := Text.substring(linkText, 0, end);
        };
        case (null) {
          return #Text line;
        };
      };
     

      Debug.print("Link text: " # linkText);

      var prior = "";
      var linkURL = "";
      var rest = "";

      // Find index of the link URL start
      switch (Text.indexOf(line, linkPatternEnd)) {
        case (?start) {
          let linkURLEnd = Text.indexOf(line, linkPatternEnd2);

          var count
          for(i in Text.toIter(line)) {
            if (i < start) {
              prior := prior + Text.fromChar(i);
            } else if (i > start) {
              rest := rest + Text.fromChar(i);
            };
          };
          
        };
        case (null) {
          return #Text line;
        };
      };

      var linkUrlTrimmed = "";
      
      // create a pattern of all the characters leading up to the link URL start
      let linkURLStartPattern = Text.substring(line, 0, linkURLStart);
      
  

      // Find index of the link URL end
      let linkURLEnd = Text.indexOf(line, linkPatternEnd, linkURLStart + 1);

      

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(linkText, spacePattern);
      Debug.print("Trimmed: " # trimmed);

     
      elements.add(#Link {
        href = linkURL;
        children = [#Text trimmed];
        title = null;
      });
  
    }

    else if (Text.contains(line, imagePattern)) {
      // Extract the link text
      var linkText = Text.trimStart(line, imagePattern);

      // Extract the link URL
      let linkURL = Text.trimEnd(linkText, linkPatternEnd);

      // Extract the link text
      linkText := Text.trimStart(linkURL, linkPatternEnd);

      // Trim any leading or trailing whitespace
      let trimmed = Text.trim(linkText, spacePattern);

      elements.add(#Image {
        src = linkURL;
        alt = trimmed;
        title = null;
      });
    };

    if(elements.size() == 0) {

      return #Text line;

    } else {
      let collection = Buffer.toArray(elements);
      return #Collection collection;

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

  func renderStaticPhrasingContentToHtml (content : [StaticPhrasingContent]) : Text {
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
