### 7. Create a Scripty Command or Macro
#### 7.1. Defining commands

**Static methods**. Scripty will scan the static methods of the class for annotations and transform each annotated method in a Command or macro.

* Most efficient commands, no extra memory needed the commands are stateless. Commands can only share information trough the context.

**Plain objects** with one or more annotated methods. Scripty will scan the instance for both instance methods with annotations and also static methods. Scripty will call the static method or the method on the instance when the command is executed.

* Instances with command methods can have shared state between the command methods. The instance command methods have access to a private memory which is only accessible by the command methods within the same instance. They also have access to the context just like the static method commands.

**Classic command instances**, implementations of the ICommand interface. This type of command can have argument list annotations to do automatic argument checking, but they cannot have parameter mapping specifications since the interface method will be called, and there is no room for negotiation. Classic commands and macros will excel in speed, since there will not be overhead of the dynamic argument handling each time it is called.

Commands can be grouped in libraries. If a command/macro is not specifically associated with a library it will be put in the "global library".

If the command or macro name is not provided, the method name will be used automatically as the name of the command or macro.

```Java
@ScriptyLibrary(name="library-name", onlyStatic=true|false)

@ScriptyCommand(name="command-name")
@ScriptyMacro(name="macro-name")
```

#### 7.2. Command arguments

Without parameter mappings the command invoker will look at the signature of the command method and provide following resources automatically. All other parameters will receive a null value.

* Object[] will receive the complete parameter list, including the command name in position 0.
* Eval will receive the expression evaluator.
* IContext will receive the current context.
* (CommandRep/MacroRepo)

#### 7.3. Macro arguments

The first list parameter will receive the expression. All other values will be set to null.

### 10. Argument Specifications

When writing your own commands, one of the tasks will be the validation of the command parameters, the types and consistency. This can be a repititive chore, the Scripty engine provides a number of annotations so that much of the checking can be done by the Scripty engine itself.

#### 10.1. Annotations

If an argument list specification will be used, the provided argument list will be converted in a normalized argument list where each argument has a fixed location, and default values will be provided. The normalized argument list will provide guarantees as specified by the argument list specification.

Two argument list types:

* @ScriptyStdArgList(name="...", fixed={...}, optional={...}, named={...})
* @ScriptyVarArgList(name="`...", fixed={...}, named={...}, vararg=..., min=..., max= ...)
* @ScriptyArgListRef(ref="...")

The argument list annotations can have an optional name, so that they can be shared and used later on. A named argument list can be specified on the class containing the commands, the command methods themselves can have the reference annotation. We need an annotation to collect several named arg lists on a command. The scope of a named argument list is a class. It will not be sharable between different classes because you cannot rely on order the commands are provided to Scripty. Changing the order could affect the available definitions, we don't want that, therefore we limit the scope of named argument lists.

**@ScriptyNamedArgLists(std={...}, var={...})**  This will only add the definitions to the scope, it will not activate an argument list. Use the reference notation for that.

The possible argument types:

* @ScriptyArg(name="param", type="TYPE-EXPR", default="literal", optional=true)

Since the annotation parameters cannot refer to objects, the default value has to be represented by a String. This will only work if the type specification can convert String instances into the type. This restriction is the result of the annotation system.

Type expressions are in fact Scripty expressions which enable us to write nested type definitions.

* Any nullAllowed=true|false
* Double
* Boolean
* Integer min=... max=...
* Byte
* String pattern="..."
* InstanceOf "classname" nullAllowed=true|false
* Long
* BigDecimal
* Float
* Short
* BigInteger


Nested expressions

* OneOf (String) (Integer) ...
* CheckedList (Integer) min=..., max=...
* InstanceOrBinding (...), it goes looking for a binding in the context and if the binding is not found, it will try to convert the value itself to the type.
* CustomType "classname", add your own custom type specification. The class should implement ITypeSpec<T>. Your implementation class should have a default constructor.
* TypeChain (String) (Integer) ..., worden na elkaar toegepast in volgorde. Dit kan nuttig zijn om eerst alles naar een string om te zetten met @ScriptyString en vervolgens de String naar iets anders om te zetten met bvb. een custom type.

#### 10.2. Examples
##### 10.2.1. Annotated static methods

```Java
@ScriptyVarArgList(
     name="IntList", 
     vararg=@ScriptyArg(type="Integer"), min=1)}
public class MyCommands {

   @ScriptyCommand(name="+")
   @ScriptyArgListRef(ref="IntList")
   public static int add(Object[] args) {...}

   @ScriptyCommand(name="-")
   @ScriptyArgListRef(ref="IntList")
   public static int sub(Object[] args) {...}

   @ScriptyCommand(name="*")
   @ScriptyArgListRef(ref="IntList")
   public static int mult(Object[] args) {...}
}
```

##### 10.2.2. Annotated command class

```Java
@ScriptyVarArgList(
     name="IntList", 
     vararg=@ScriptyVarArg(
          type=@ScriptyInteger, 
          min= 1))
public class Add {
   @ScriptyCommand(name="+")
      @ScriptyArgListRef(ref="IntList")
   public Object add(Object[] aArgs) {...}
}
```

### 11. Parameter Mapping

These annotations are here to delegate the parameter mapping to the Scripty engine. Since Scripty is aware of the command line structure, it can map the parameters to the parameters of the command implementation.

#### 11.1. Annotations

Parameter mapping can be done if there is an argument list specification which names the arguments. The names can be used while annotating the command parameters.

Certain types will be recognized automatically by Scripty, these don't have to be annotated.

* IEval
* IContext
* Object[], will receive the complete argument list automatically.

**@ScriptyParam("name")**. Will lookup the name in the argument specification. We do not need to introduce a separate annotation for the variable part when using variable argument lists, it can be named just like the other parameters. 

**@ScriptyContextParam("name", unboundException=false)** will get a specific binding from the context. The behavior if the parameter is not present will depend on the unboundException setting. If unboundException is false, a null value will be passed if the binding was not present. Otherwise an exception will be thrown.

These method annotations define what to do with the return value. Only one of these can be present. If the annotation is present, the return value will be bound automatically in the context.

**@ScriptyDefBinding("name")** The "def" wil be used, it will create a new binding or it will overwrite an existing binding.

**@ScriptySetBinding("name")** It will change an existing binding, it will throw an exception if the binding does not exist.

#### 11.2. Examples

```
@ScriptyStdArgList(
   name="testlist",
   fixed={
      @ScriptyArg(name="uno", type="String"),
      @ScriptyArg(name="duo", type="String")})
class CommandLib1 
{
    @ScriptyArgListRef("testlist")
    public Object myCmd(@ScriptyParam("uno") String one, 
                        @ScriptyParam("duo") String two)
    {
      return null;
    }
}
```