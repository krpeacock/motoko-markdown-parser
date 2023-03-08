import Text "mo:base/Text";
import Char "mo:base/Char";
import Bool "mo:base/Bool";
import Debug "mo:base/Debug";
import Iter "mo:base/Iter";

module {
  public func textIndexOf(text : Text, pattern : Text.Pattern) : Int {
    var patternText = "";
    switch (pattern) {
      case (#text t) {
        patternText := t;
      };
      case (#char c) {
        patternText := Text.fromChar(c);
      };
      case (_) { return -1 };

    };

    let contains = Text.contains(text, pattern);
    Debug.print("Contains: " # debug_show contains);

    if (patternText.size() == 0 or patternText.size() > text.size()) {
      return -1;
    };

    for (i in Iter.range(0, text.size() - patternText.size())) {
      let sub = textSubstring(text, i, patternText.size());
      if (sub == patternText) {
        return i;
      };
    };

    return -1;
  };

  public func textSubstring(text : Text, start : Int, length : Int) : Text {
    var output = "";
    var count = 0;
    for (char in text.chars()) {
      if (count >= start and count < start + length) {
        output := output # Text.fromChar(char);
      };
      count := count + 1;
    };
    output;
  };
};
