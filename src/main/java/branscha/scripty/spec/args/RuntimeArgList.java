package branscha.scripty.spec.args;

import branscha.scripty.spec.map.ArgMapping;

import java.util.Map;

/**
 * The argument lists declarations are compiled into this data structure which is then used at runtime by the
 * Scripty eval to do the type conversions and checking. We also remember the way to obtain named arguments from
 * the argument list, we remember in which location we store them so if we need to map them to a command parameter
 * later on we can easily fetch it from the argument array.
 */
public class RuntimeArgList {

    /**
     * The argument list that knows how to convert and check a list of arguments.
     */
    private ArgList argList;

    /**
     * Mappers to obtain named arguments from the argument list.
     */
    private Map<String, ArgMapping> argMappings;

    public RuntimeArgList(ArgList argList, Map<String, ArgMapping> argMappings) {
        this.argList = argList;
        this.argMappings = argMappings;
    }

    public ArgList getArgList() {
        return argList;
    }

    public Map<String, ArgMapping> getArgMappings() {
        return argMappings;
    }
}
