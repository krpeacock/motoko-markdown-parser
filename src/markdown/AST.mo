// Defines the nodes in an Abstract Syntax Tree (AST) for markdown

module {

  public type Node = { kind : Text; value : Text; children : [Node] };

  public type Parent = {
    children : [Node];
  };

  public type Literal = {
    value : Text;
  };

  public type Paragraph = {
    kind : "paragraph";
    children : [Node];
  };

};
