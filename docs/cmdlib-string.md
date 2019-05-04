# String Commands

**str?**

Check if an arbitrary object is a string.

**str-trim**

Trim the whitespace on both ends of the string.

**str-format**

It has the same behaviour as the Java version.

```
> print (str-format "%s = %s" "1 + 1" (+ 1 1))
1 + 1 = 2
```

**str-match**

Do a single match, the result is a list of matched groups. Even is there is no group in the pattern, the global match 
is always available.

```
> print (str-match "rosa\S*\b" "rosa rosam rosas")
[rosa]
```

**str-match***

Repeatedly match the pattern. The result is a list of lists of matches. It is the same as str-match but it is applied 
repeatedly to the string.

```
> print (str-match* "rosa\S*\b" "rosa rosam rosas")
[[rosa], [rosam], [rosas]]
```

**str-match?**

Check if a string complies to a pattern. The result is a boolean.
