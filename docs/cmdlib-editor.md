<h2><a name="edit" id="edit">edit</a></h2>

<p>Edit a text value in a multi-line text editor. It is often easier for longer
texts to use the editor. The result is a String when the edit is accepted or
null if rejected.</p>

<p><code>(edit "Title" "Initial value")</code></p>

<h2><a name="edit-expr" id="edit-expr">edit-expr</a></h2>

<p>Edit a single Scripty expression in a multi-line text editor. The result is
a single parsed expression. If multipe expressions are written in the text
pane, only the first one will be returned.</p>

<p><code>(edit-expr "Title" "Initial value").</code></p>

