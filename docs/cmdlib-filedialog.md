


    @ScriptyCommand(name="file-dialog")
    @ScriptyVarArgList(
            vararg = @ScriptyArg(name="types", type="String"),
            named = {
                @ScriptyArg(name="title", type="String", value = "Select a file", optional = true), 
                @ScriptyArg(name="type", type = "Enum file dir", value = "file", optional = true),
                @ScriptyArg(name="mode", type="Enum open save", value = "open", optional = true)})
    public  File fileDialog(
            @ScriptyParam("title") String aTitle, 
            @ScriptyParam("type") String aType, 
            @ScriptyParam("mode") String aMode,
            @ScriptyParam("types") Object[] aTypes)


===> Onderstaande cmds zijn niet meer geldig ...


Commands to open a dialog to open or save a file.

<h2><a name="choose-fil" id="choose-fil">choose-file</a></h2>

<p>Open a GUI file selector so that you can choose a file interactively in
stead of writing down the full pathname. It makes file selection easier. If you
add a list of extensions, don't forget to quote the list, or otherwise the
evaluator will try to execute the list.</p>

<p><code>(choose-file "Dialog Title" '(lsp jar ...))</code></p>

<h2><a name="choose-dir" id="choose-dir">choose-dir</a></h2>

<p>Open a GUI file selector so that you can choose a directory interactively in
stead of writing down the full pathname. It makes directory selection
easier.</p>
