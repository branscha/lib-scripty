
# File Dialog Command (GUI)

Open a GUI file selector so that you can choose a directory interactively in
stead of writing down the full pathname. It makes directory selection
easier.</p>

**file-dialog**

Open the Java file dialog to select a file or a directory. The dialog can be customized using 
the various options.

```
file-dialog *.md *.txt [type=file*|dir] [mode=open*|save] [title=Title]
```

* The arguments *.md, *.txt will be used as filter patterns in the dialog box. There can be 0 or more patterns.
* **type**: Indicates whether files or directories can be selected. The default is file.
* **mode**: Indicates if the dialog selection will be used for reading or saving the file. The file dialog selection
button will be according to the mode.
* **title**: The title of the dialog. The default is "Select a file".