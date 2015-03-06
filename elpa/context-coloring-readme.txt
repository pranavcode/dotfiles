Highlights code according to function context.

- Code in the global scope is one color.  Code in functions within the global
  scope is a different color, and code within such functions is another
  color, and so on.
- Identifiers retain the color of the scope in which they are declared.

Lexical scope information at-a-glance can assist a programmer in
understanding the overall structure of a program.  It can help to curb nasty
bugs like name shadowing.  A rainbow can indicate excessive complexity. State
change within a closure is easily monitored.

By default, Context Coloring still highlights comments and strings
syntactically.  It is still easy to differentiate code from non-code, and
strings cannot be confused for variables.

To use, add the following to your ~/.emacs:

(require 'context-coloring)
(add-hook 'js2-mode-hook 'context-coloring-mode)

js-mode or js3-mode support requires Node.js 0.10+ and the scopifier
executable.

$ npm install -g scopifier
