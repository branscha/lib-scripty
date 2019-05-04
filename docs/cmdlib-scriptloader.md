# Load Script Command

Load and reload script files into the environment.

**load**

Load and execute one or more scripts. If the file exists and is readable, it will be remembered for the reload.

```
(load file | classpath:/resource | cp:/resource ...)
```

**reload**

Reload previously loaded files. A file is remembered if it existed and was readable.

```
(reload)
```

