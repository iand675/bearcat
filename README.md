# bearcat

![Bearcat](bearcat.jpg)

The beginnings of a cool frontend framework thing.


Built upon Google's incremental-dom library & GHCJS.

Features:

- Blaze-like / Lucid-like HTML DSL.
- Somewhat correct-by-construction HTML. Bearcat prevents you from using HTML element attributes
  upon elements that don't respect the given attribute. Future goals are to go even further by ensuring that use of attributes like `selected` correctly respect whether they are being used by an element that allows multi-selection or only allows a single value to be selected.
- A layered approach to extensibility. Want to just use some nice wrappers around Google's extensible-dom library without using the fancy stuff on top of that? Go ahead.
- Transactional, reactive updates. Using the `Reactor` module opens up the ability to conveniently rerender the DOM incrementally simply by updating a `Reactor` with new state.

Future work:

- Managed DOM event handlers
- Surrogate rendering keys
- Type-level in-app routing
- Scoped DOM diffing. Instead of patching the entire app, patch only the smallest sections necessary to bring the app-state up to date.
