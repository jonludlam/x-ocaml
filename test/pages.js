// The pages run_tests.js runs and serve.js lists, with what each is for.
exports.TESTS = [
  ['/test_modes.html', 'Cell modes: hidden, interactive, exercise, test; theming variables'],
  ['/test_exercises.html', 'Saved exercises: a kept version, Reset'],
  ['/test_stop.html', 'Run and Stop'],
  ['/test_links.html', 'Test linking, the awkward cases'],
  ['/test_saved_edge.html', 'Saved exercises, the awkward cases'],
  ['/test_storage_denied.html', 'No storage at all'],
  ['/test_storage_full.html', 'Storage full'],
  ['/test_running_edge.html', 'Running and stopping, the awkward cases'],
  ['/test_flood.html', 'Output without end'],
  ['/test_many_lines.html', 'Twenty thousand lines of output'],
  ['/test_text.html', 'Awkward text and silly code'],
  ['/test_script_attrs.html', 'Script attributes: elt-name, run-on, query strings'],
  ['/test_no_worker.html', 'A script without src-worker'],
  ['/sub/test_base.html', 'A base URL elsewhere'],
  ['/test_jtw.html', 'The js_top_worker backend'],
  ['/test_widgets.html', 'Widgets: events, patching in place, placement, adapters'],
];

exports.EXAMPLES = [
  ['/examples/tutorial.html', 'A short tutorial: prose, hidden helper, exercises and tests'],
  ['/examples/running.html', 'Running and stopping'],
  ['/examples/on_click.html', 'Nothing runs until asked, and another element name'],
  ['/examples/theming.html', 'A dark theme through the --xo-* variables'],
  ['/examples/text.html', 'Text of all sorts'],
  ['/examples/widgets.html', 'Widgets: a counter, inputs, SVG, a list and a managed canvas'],
];
