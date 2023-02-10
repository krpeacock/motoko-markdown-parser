import Parser "MarkdownParser";

actor markdown {
  public func parse(md : Text) : async Parser.ParseResult {
    Parser.parse(md);
  };
};
