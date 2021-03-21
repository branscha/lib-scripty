# Editor Commands (GUI)
## edit

Edit a text value in a multi-line text editor. It is often easier for longer
texts to use the editor. The result is a String when the edit is accepted or
null if rejected.

```
edit "Title" "Initial value"
```

## edit-expr

Edit a single Scripty expression in a multi-line text editor. The result is
a single parsed expression. If multipe expressions are written in the text
pane, only the first one will be returned.

```
edit-expr "Title" "Initial value"
```

