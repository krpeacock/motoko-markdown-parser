## Motoko Markdown Parser

This is a simple markdown parser written in Motoko. It is based on the [CommonMark Spec](https://spec.commonmark.org/0.29/) and an approximation of the [Mdast AST](https://github.com/syntax-tree/mdast).

You can install it with Mops, although it is not yet feature complete.

```bash
mops add markdown
```

### Usage

```motoko
import Markdown "mo:markdown";

let md = "# Hello World";
let html = Markdown.parse(md);
```

You can also parse to an AST, which will allow you to do more complicated things with the markdown, such as rendering to custom html, web components, or other formats.

```motoko
import Markdown "mo:markdown";

let md = "# Hello World";
let ast = Markdown.parseToAst(md);
```

### Development

To run the tests, you can use the `test` or `watch` script in the Makefile. It will build the project and run the tests.

```bash
make test
```

```bash
make watch
```

### Contributing

Contributions are welcome! Please open an issue or PR if you find a bug or have a feature request.

### License

MIT

### Roadmap and Checklist

- [ ] Add support for all block elements

  - [x] Paragraph
  - [x] Heading
  - [ ] Thematic Break
  - [ ] Blockquote
  - [ ] List
  - [ ] Code
  - [ ] HTML
  - [ ] Table
  - [ ] Definition
  - [ ] Footnote Definition
  - [ ] Comment

- [ ] Add support for all inline elements

  - [ ] Text
  - [ ] Softbreak
  - [ ] Hardbreak
  - [ ] Emphasis
  - [ ] Strong
  - [ ] Delete
  - [ ] Link
  - [ ] Image
  - [ ] InlineCode
  - [ ] HTML
  - [ ] LinkReference
  - [ ] ImageReference
  - [ ] FootnoteReference
  - [ ] Footnote
  - [ ] Break
  - [ ] InlineMath
  - [ ] DisplayMath
  - [ ] Inline Comment

- [ ] Support for inline HTML

- [ ] Support for custom renderers

- [ ] Planned Extensions
  - [ ] IDs and classes
  - [ ] Support for frontmatter
  - [ ] Automatic links for headings
