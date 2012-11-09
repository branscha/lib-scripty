# File System Commands

If you want to give the user of the embedding application the possibilities of navigating the local file system you can activate this command library. 

* It is intentionally based on the Java File object (for re-usability in other commands). As a consequence, a move only accepts 2 files, no wildcards.
* No wild card globbing by the shell. The ls provides a grep on the short name. The find command provides a lookup facility.
* Delete was not implemented for safety reasons.

## Command Reference

**cd**

Change the current directory.

**pwd**

Print the current directory and return the File object.

**ls**

Print a listing of the current directory and return it as an array.

* grep=regexp: applied to the absolute pathname.
* quiet=true|*false.
* files=*true|false : include files.
* dirs=*true|false : include directories.
* exec=lambda: process the files using a lambda. Processing is done after grepping and filtering.

**cat**

Not implemented.

**rslv**

Resolve is the same as a 'quiet' pwd, but pwd only returns directories, whereas rslv can also resolve to files. This command is *very* important for integration with other libraries. The other command libraries should not be dependent on this one, they can simply request path string and this command can resolve the path to the File object.

**mv**

Rename a file. It does not use wild cards since the implementation is based on the Java File class which does not accept wild cards.
