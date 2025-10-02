# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

x-ocaml is a WebComponent-based OCaml notebook system that embeds interactive OCaml editors in web pages. The system compiles OCaml to JavaScript using js_of_ocaml and runs the OCaml toplevel, Merlin (for IDE features), and OCamlformat entirely in the browser via Web Workers.

## Build Commands

### Building the project

Always use the `release` profile for production builds to optimize JavaScript file size:

```bash
dune build --profile=release
```

This generates three JavaScript files in the project root:
- `x-ocaml.js` - Main WebComponent script
- `x-ocaml.worker+effects.js` - Worker with effects support
- `x-ocaml.worker.js` - Worker without effects

For development builds:

```bash
dune build
```

### Installing dependencies

With dune (recommended):
```bash
dune pkg lock
```

With opam:
```bash
opam update && opam install . --deps-only
```

### Formatting

The project uses ocamlformat version 0.27.0. Format code with:

```bash
dune build @fmt --auto-promote
```

## Architecture

### Main Components

**Client-Worker Architecture**: The system uses a message-passing architecture between the main thread and a Web Worker:

- **Client Side** ([src/](src/)) - Main thread WebComponent implementation
  - [src/x_ocaml.ml](src/x_ocaml.ml) - Entry point, initializes worker and manages cell registry
  - [src/webcomponent.ml](src/webcomponent.ml) - Custom element registration and shadow DOM setup
  - [src/cell.ml](src/cell.ml) - Individual notebook cell management
  - [src/editor.ml](src/editor.ml) - CodeMirror editor integration
  - [src/client.ml](src/client.ml) - Worker communication client

- **Worker Side** ([worker/](worker/)) - Runs OCaml toplevel and tooling
  - [worker/x_worker.ml](worker/x_worker.ml) - Worker message handler
  - [worker/eval.ml](worker/eval.ml) - OCaml toplevel execution
  - [worker/ocamlfmt.ml](worker/ocamlfmt.ml) - OCamlformat integration
  - Two build variants: [worker/effects/](worker/effects/) (with effects) and [worker/no-effects/](worker/no-effects/)

- **Protocol** ([protocol/x_protocol.ml](protocol/x_protocol.ml)) - Message types for client-worker communication
  - Requests: `Merlin`, `Eval`, `Format`, `Format_config`, `Setup`
  - Responses: `Merlin_response`, `Top_response`, `Top_response_at`, `Formatted_source`

### Vendored Dependencies

The repository vendors two critical dependencies as Git submodules:

- **jsoo-code-mirror** - OCaml bindings to CodeMirror 6 using brr
- **merlin-js** - JavaScript-compiled Merlin for IDE features (autocomplete, type hints, navigation)

### Build System

**Dune rules** ([dune](dune)) promote compiled JavaScript files from `_build/` to the project root. The main dune file defines:
- File promotion rules for the three JS outputs
- Integrity hash generation for the README (runs on release profile)

**Package export tool** ([bin/x_ocaml.ml](bin/x_ocaml.ml)) - A command-line utility that uses ocamlfind to:
1. Discover OCaml library dependencies
2. Compile each library to JavaScript with js_of_ocaml
3. Bundle them into a single loadable JavaScript file
4. Handle both regular libraries and PPX extensions

### Custom Attributes

The `<x-ocaml>` script tag supports these attributes:
- `src-worker` (required) - URL to the worker JavaScript file
- `src-load` - Additional libraries to load
- `src-style` - External CSS file for styling
- `inline-style` - Inline CSS styles
- `x-ocamlformat` - Custom ocamlformat configuration
- `elt-name` - Custom element name (default: "x-ocaml")

Individual `<x-ocaml>` elements support:
- `nomerlin` - Disable Merlin integration for that cell

## Development Notes

### Working with the worker

When modifying worker code, remember:
- Messages are marshaled between client and worker (see [protocol/x_protocol.ml](protocol/x_protocol.ml))
- The worker runs the OCaml toplevel using `js_of_ocaml-toplevel`
- Effects support is optional and controlled by the `--effects` flag during compilation

### Modifying the protocol

When changing the message protocol:
1. Update types in [protocol/x_protocol.ml](protocol/x_protocol.ml)
2. Update message handling in [worker/x_worker.ml](worker/x_worker.ml)
3. Update client-side handlers in [src/x_ocaml.ml](src/x_ocaml.ml)

### Styling

CSS is embedded via ppx_blob in [src/style.css](src/style.css) and loaded as a preprocessor dependency in [src/dune](src/dune).

### Testing changes

Create a simple HTML file to test your changes:

```html
<script async
  src="./x-ocaml.js"
  src-worker="./x-ocaml.worker+effects.js"
></script>
<x-ocaml>let x = 42</x-ocaml>
```

Then serve it with a local HTTP server (the worker requires proper CORS/origin handling).
