# Fox lisp JS

This is a fox lisp implementation for the web. It can be used with node.

to run tests, do:

./fox test2.lisp

```
run:

node test.js

npm run build

```



## Todo
- Figure out a way to do UIs. At this moment there is basic text rendering using canvas. Maybe that could be extended for full UI rendering.
  1. Measure the size of the text.
  2. Figure out how to break up text.
  3. Start working with panels.
- CSG:
  - Split a polygon based on some plane.
  - A cube consists of 6 planes. Cutting those in the right way and recombining what is left should allow for csg with cube parts. I hope this can be done without exploding geometric complexity.
- Cleanup
 - Create foxlisp npm
 - Split out game code
 