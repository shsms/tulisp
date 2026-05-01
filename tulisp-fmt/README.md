# tulisp-fmt

Source code formatter for [tulisp] / Emacs Lisp. Round-trip stable
(`format(format(x)) == format(x)`), zero external dependencies, and
intentionally independent of the `tulisp` interpreter — it operates on
source text alone.

[tulisp]: https://crates.io/crates/tulisp

## Install

```sh
cargo install tulisp-fmt
```

This installs the `tulisp-fmt` binary. Embedders that only want the
library can disable the binary feature:

```toml
[dependencies]
tulisp-fmt = { version = "0.1", default-features = false }
```

## CLI

```text
tulisp-fmt [OPTIONS] [FILE|DIR...]

Format Emacs Lisp / tulisp source files.

With no FILE argument, reads from stdin and writes to stdout.
With FILE arguments, formats each file and writes the result to stdout
by default. Pass `-w` to overwrite the file in place. Directory
arguments are walked recursively for *.lisp and *.el files; entries
starting with `.` (e.g. .git) are skipped.

Style settings can also live in a .tulisp-fmt.toml file discovered by
walking up from the input. CLI flags always override the file.

Options:
  -w, --write          write the formatted result back to each FILE
      --check          exit 1 (and list filenames) if any FILE is unformatted
  -d, --diff           print a unified diff of source vs. formatted; exit 1 if any differ
      --width N        wrap lists past column N (default 80)
      --indent-width N body-indent step in columns (default 2)
      --use-tabs       emit tab characters for indents instead of spaces
      --tab-width N    columns per tab when --use-tabs is set (default 8)
      --range S:E      format only top-level forms overlapping byte range [S, E)
  -h, --help           print this help and exit
  -V, --version        print version and exit
```

Exit codes:

- `0` — success / no changes needed
- `1` — files differ from formatted (`--check` / `--diff`) or parse error from stdin
- `2` — usage error or I/O error

## Library

```rust
use tulisp_fmt::{format, format_with_width, format_with_style, format_range, Style};

// Default style (80-col, two-space indent).
let out = format("(let ((x 1) (y 2)) (+ x y))").unwrap();

// Custom width.
let out = format_with_width("(very long form ...)", 100).unwrap();

// Full style control.
let style = Style {
    width: 100,
    indent_width: 4,
    use_tabs: true,
    tab_width: 4,
};
let out = format_with_style(src, &style).unwrap();

// Format only top-level forms touching [start, end), preserving the rest.
let out = format_range(src, 120, 200, &Style::default()).unwrap();
```

The `parse` function returns the underlying CST so tooling can
inspect the tree directly:

```rust
let cst = tulisp_fmt::parse(src).unwrap();
for node in &cst.nodes {
    /* ... */
}
```

Errors are [`parse::ParseError`]s; their `render` method produces a
`file:line:col` diagnostic with a caret-pointing source snippet.

## Configuration file

The CLI looks for `.tulisp-fmt.toml` by walking upward from each
input file (or the current working directory for stdin). The file is
a flat-key TOML subset:

```toml
# .tulisp-fmt.toml
width = 100
indent_width = 2
use_tabs = false
tab_width = 8
```

Unknown keys are an error so typos are loud. CLI flags always win
over file settings — unset CLI flags fall through to the file.

## Layout rules at a glance

- Lists that fit within the width budget at their starting column
  render on one line; lists that don't render multi-line.
- User line breaks and comments are preserved and force multi-line
  layout for the surrounding list.
- A blank line is enforced after every top-level `defun`, `defmacro`,
  `defvar`, `defconst`, `defspecial`.
- Bindings of `let` / `let*` get one binding per line whenever the
  bindings list goes multi-line.
- `(declare (indent N))` on a top-level `defmacro` is harvested in a
  pre-pass; calls to that macro indent according to N for the rest of
  the file.
- Consecutive lines ending with a `;`-comment are aligned so the
  semicolons line up.

The tests under `tulisp-fmt/tests/parity.rs` lock in byte-for-byte
parity with Emacs `lisp-data-mode` for a representative corpus.

## License

GPL-3.0, matching the rest of the [tulisp] workspace.
