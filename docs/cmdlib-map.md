Manipulate map data structures.

**map-create**

Create an empty map. You can immediately insert pairs.

(map-create key=val | str | m ...)

**map?**

Test whether an object is a map.

```
(map? m)
```

**map-set**

Insert the key/value in a map.

```
(map-set m key val)
```

**map-get**

Get the value bound by the key.

```
(map-get m key)
```

**map-key?**

Test whether a key is present.

```
(map-key? m key)
```

**map-keys**

Get the list of keys.

```
(map-keys m)
```

**map-values**

Get the list of values.

```
(map-values m)
```

**map-clear**

Make the map empty.

```
(map-clear m)
```

**map-size**

Get the number of entries in the map.

```
(map-size m)
```

