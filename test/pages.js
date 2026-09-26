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
  ['/sub/test_worker_relative.html', 'A page below the root: its worker named ../x-ocaml.worker.js'],
  ['/sub/test_worker_absolute.html', 'A page below the root: its worker named /x-ocaml.worker.js'],
  ['/test_elements.html', 'Other custom elements on the page'],
  ['/test_big_module.html', 'A module of many definitions'],
  ['/test_displays.html', 'Displays and callbacks: what cells show on the page'],
];

exports.EXAMPLES = [
  ['/examples/tutorial.html', 'A short tutorial: prose, hidden helper, exercises and tests'],
  ['/examples/running.html', 'Running and stopping'],
  ['/examples/on_click.html', 'Nothing runs until asked, and another element name'],
  ['/examples/theming.html', 'A dark theme through the --xo-* variables'],
  ['/examples/text.html', 'Text of all sorts'],
];
