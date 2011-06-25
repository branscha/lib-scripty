/*
 * Scripty Programming Language
 * Copyright (C) 2010-2011 Bruno Ranschaert, S.D.I.-Consulting BVBA
 * http://www.sdi-consulting.be
 * mailto://info@sdi-consulting.be
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package com.sdicons.repl.cmdlib;

import com.sdicons.repl.parser.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/** Map command module.
 *
 * <ul>
 *    <li><b><code>map-create</code></b> Create an empty map. You can immediately insert pairs. <code>(map-create key=val | str | m ...)</code></li>
 *    <li><b><code>map?</code></b> Test whether an object is a map.<code>(map? m)</code></li>
 *    <li><b><code>map-set</code></b> Insert the key/value in a map. <code>(map-set m key val)</code></li>
 *    <li><b><code>map-get</code></b> Get the value bound by the key. <code>(map-get m key)</code></li>
 *    <li><b><code>map-key?</code></b> Test whether a key is present. <code>(map-key? m key)</code></li>
 *    <li><b><code>map-keys</code></b> Get the list of keys. <code>(map-keys m)</code></li>
 *    <li><b><code>map-values</code></b> Get the list of values. <code>(map-values m)</code></li>
 *    <li><b><code>map-clear</code></b> Make the map empty. <code>(map-clear m)</code> </li>
 *    <li><b><code>map-size</code></b> Get the number of entries in the map. <code>(map-size m)</code></li>
 * </ul>
 *
 */
public class MapCmd
extends AbstractCommand
{
    public static enum MapCmdType {mapSize, mapCreate, isMap, mapSet, mapGet, isMapKey, mapKeys, mapValues, mapClear};

    private static final int IDX_CMD = 0;
    private static final int IDX_MAP = 1;
    private static final int IDX_KEY = 2;
    private static final int IDX_VAL = 3;

    private MapCmdType type;

    public MapCmd(MapCmdType aType)
    {
        type = aType;
    }

    public static void registerCommands(IRegistry aReg)
    {
        aReg.registerCommand("map-create", new MapCmd(MapCmdType.mapCreate));
        aReg.registerCommand("map?", new MapCmd(MapCmdType.isMap));
        aReg.registerCommand("map-set", new MapCmd(MapCmdType.mapSet));
        aReg.registerCommand("map-get", new MapCmd(MapCmdType.mapGet));
        aReg.registerCommand("map-key?", new MapCmd(MapCmdType.isMapKey));
        aReg.registerCommand("map-keys", new MapCmd(MapCmdType.mapKeys));
        aReg.registerCommand("map-values", new MapCmd(MapCmdType.mapValues));
        aReg.registerCommand("map-clear", new MapCmd(MapCmdType.mapClear));
        aReg.registerCommand("map-size", new MapCmd(MapCmdType.mapSize));
    }

    @SuppressWarnings("unchecked")
    private Map guardSingleMap(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 2 || !(aArgs[IDX_MAP] instanceof Map))
            throw new CommandException(String.format("ERROR: Command '%s' expects a single map argument.", aArgs[IDX_CMD]));
        return (Map) aArgs[IDX_MAP];
    }

    private Object guardSingleObject(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 2)
            throw new CommandException(String.format("ERROR: Command '%s' expects a single argument.", aArgs[IDX_CMD]));
        return aArgs[1];
    }

    @SuppressWarnings({ "unchecked" })
    private Map guardMapKey(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 3)
            throw new CommandException(String.format("ERROR: Command '%s' expects two arguments, a map and key.", aArgs[IDX_CMD]));
        if(!(aArgs[IDX_MAP] instanceof Map))
            throw new CommandException(String.format("ERROR: Command '%s' expects a map as a first argument.", aArgs[IDX_CMD]));
        if(aArgs[IDX_KEY] == null)
            throw new CommandException(String.format("ERROR: Command '%s' expects a non null key.", aArgs[IDX_CMD]));
        return (Map) aArgs[IDX_MAP];
    }

    @SuppressWarnings({ "unchecked" })
    private Map guardMapKeyValue(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 4)
            throw new CommandException(String.format("ERROR: Command '%s' expects three arguments, a map, key and value.", aArgs[IDX_CMD]));
        if(!(aArgs[IDX_MAP] instanceof Map))
            throw new CommandException(String.format("ERROR: Command '%s' expects a map as a first argument.", aArgs[IDX_CMD]));
        if(aArgs[IDX_KEY] == null)
            throw new CommandException(String.format("ERROR: Command '%s' expects a non null key.", aArgs[IDX_CMD]));
        return (Map) aArgs[IDX_MAP];
    }

    @SuppressWarnings("unchecked")
    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        switch(type)
        {
            case mapCreate:
                {
                    Map lMap = new HashMap();
                    for(int i = 1; i < aArgs.length; i++)
                    {
                        if(aArgs[i] instanceof String)
                        {
                            lMap.put(aArgs[i], null);
                        }
                        else if(aArgs[i] instanceof Pair)
                        {
                            final Pair lPair = (Pair) aArgs[i];
                            lMap.put(lPair.getLeft(), lPair.getRight());
                        }
                        else if(aArgs[i] instanceof Map)
                        {
                            lMap.putAll((Map) aArgs[i]);
                        }
                        else
                        {
                            Object lCulprit = aArgs[i];
                            throw new CommandException(String.format("ERROR: Command '%s' expects zero or more string or pairs.\nArgument %d is of type '%s'.", aArgs[IDX_CMD], i, lCulprit==null?"null":lCulprit.getClass().getCanonicalName()));
                        }
                    }
                    return lMap;
                }
            case isMap:
                return (guardSingleObject(aArgs) instanceof Map);
            case mapKeys:
                return new ArrayList(guardSingleMap(aArgs).keySet());
            case mapValues:
                return new ArrayList(guardSingleMap(aArgs).values());
            case mapClear:
            {
                Map lMap = guardSingleMap(aArgs);
                lMap.clear();
                return lMap;
            }
            case mapSet:
                return guardMapKeyValue(aArgs).put(aArgs[IDX_KEY], aArgs[IDX_VAL]);
            case mapGet:
            {
                Map lMap = guardMapKey(aArgs);
                if(!lMap.containsKey(aArgs[IDX_KEY]))
                    throw new CommandException(String.format("ERROR: Command '%s', the property '%s' does not exist.", aArgs[0], aArgs[IDX_CMD]));
                return lMap.get(aArgs[IDX_KEY]);
            }
            case isMapKey:
                return guardMapKey(aArgs).containsKey(aArgs[IDX_KEY]);
            case mapSize:
                return guardSingleMap(aArgs).size();
            default:
                throw new CommandException(String.format("ERROR: Command '%s' internal error.", aArgs[IDX_CMD]));
        }
    }
}
