package branscha.scripty.cmdlib;

import branscha.scripty.ExtensionException;
import branscha.scripty.ProcessorException;
import branscha.scripty.ScriptyStreamProcessor;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;


public class BeanLibraryTest {

    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(MathLibrary.class, BeanLibrary.class);
    }

    @Test
    public void testIt()
    throws ProcessorException {
        class MyBean {
            private String data = "Bruno";
            private int[] intList = new int[]{1, 2, 5, 7, 9, 11, 13};
            private List<String> strList = Arrays.asList("uno", "duo", "tres");
            private Map<String, String> map = new HashMap<>();

            public MyBean() {
                map.put("abc", "def");
                map.put("123", "456");
            }

            public String getData() {
                return data;
            }

            public void setData(String data) {
                this.data = data;
            }

            public int[] getIntList() {
                return intList;
            }

            public void setIntList(int[] intList) {
                this.intList = intList;
            }

            public List<String> getStrList() {
                return strList;
            }

            public void setStrList(List<String> strList) {
                this.strList = strList;
            }

            public String callMe() {
                return "Hi there.";
            }

            public Map<String, String> getMap() {
                return map;
            }

            public void setMap(Map<String, String> map) {
                this.map = map;
            }
        }

        scripty.getContext().defBinding("test", new MyBean());

        // Verify the current directory which should be /  at the start  of scripty.
        Object result = scripty.process("bean-pwd");
        assertEquals("/", result);

        // List the current directory. The context should contain our test instance.
        // Note that we removed the hash for different test runs.
        result = scripty.process("bean-ls");
        assertEquals("Directory{\n" +
                "type=BasicContext\n" +
                "dir=\n" +
                "    *error               PrintWriter                RW\n" +
                "    *input               LineNumberReader           RW\n" +
                "    *output              PrintWriter                RW\n" +
                "    test                 MyBean                     RW\n" +
                "}", result.toString());

        // Step into the bean, the result of the cd is also that bean.
        result = scripty.process("bean-cd test");
        assertThat(result.toString(), containsString("branscha.scripty.cmdlib.BeanLibraryTest$1MyBean"));

        // Current path must have changed.
        result = scripty.process("bean-pwd");
        assertEquals("/test", result);

        // We are currently located in the bean instance.
        result = scripty.process("bean-ls");
        assertEquals("Directory{\n" +
                "type=MyBean\n" +
                "dir=\n" +
                "    class                Class                      R-\n" +
                "    data                 String                     RW\n" +
                "    intList              int[]                      RW\n" +
                "    map                  Map                        RW\n" +
                "    strList              List                       RW\n" +
                "}", result.toString());

        // Show the contents of a property.
        result = scripty.process("bean-cat data");
        assertEquals("Bruno", result);

        // Call a method.
        result = scripty.process("bean-call . callMe");
        assertEquals("Hi there.", result);

        // Step into the bean, the result of the cd is also that bean.
        result = scripty.process("bean-cd intList");
        result = scripty.process("bean-pwd");
        assertEquals("/test/intList", result);

        result = scripty.process("bean-ls");
        assertEquals("Directory{\n" +
                "type=int[]\n" +
                "dir=\n" +
                "    [0]                  Integer                    RW\n" +
                "    [1]                  Integer                    RW\n" +
                "    [2]                  Integer                    RW\n" +
                "    [3]                  Integer                    RW\n" +
                "    [4]                  Integer                    RW\n" +
                "    [5]                  Integer                    RW\n" +
                "    [6]                  Integer                    RW\n" +
                "}", result.toString());

        result = scripty.process("bean-cat 3");
        assertEquals(7, result);

        scripty.process("bean-cd ../strList/2");
        result = scripty.process("bean-cat .");
        assertEquals("tres", result);

        result = scripty.process("bean-get /test/data");
        assertEquals("Bruno", result);
        scripty.process("bean-set /test data=Alexander");
        result = scripty.process("bean-get /test/data");
        assertEquals("Alexander", result);

        result = scripty.process("bean-get /test/intList/5");

        assertEquals(11, result);
        scripty.process("bean-set /test/intList 5=(bean-get /test/intList/6)");
        result = scripty.process("bean-get /test/intList/5");
        Object result2 = scripty.process("bean-get /test/intList[5]");
        assertEquals(13, result);
        assertEquals(result, result2);

        result = scripty.process("bean-get /test/strList/1");
        assertEquals("duo", result);
        scripty.process("bean-set /test/strList 1=Ok");
        result = scripty.process("bean-get /test/strList/1");
        assertEquals("Ok", result);

        scripty.process("bean-cd /test/map");
        result = scripty.process("bean-ls");
        assertEquals("Directory{\n" +
                "type=HashMap\n" +
                "dir=\n" +
                "    [123]                String                     RW\n" +
                "    [abc]                String                     RW\n" +
                "}", result.toString());

    }
}