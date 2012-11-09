    @ScriptyCommand(name="rec-edit")
    @ScriptyStdArgList(fixed={@ScriptyArg(name="fields", type ="ListOf(String)"), @ScriptyArg(name="values", type="ListOf(Any)")})
    public static List recEdit(@ScriptyParam("fields") List<String> aPropNames, @ScriptyParam("values") List<Object> aPropVals)
    throws CommandException

<h2><a name="edit-rec" id="edit-rec">edit-rec</a></h2>

<p>Open a GUI editor for name/value pairs.</p>

<p><code>(edit-rec '(&lt;fields&gt;) '(values))</code></p>
