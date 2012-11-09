# Math Commands

The JavaScript approach was taken to implement the math command library, there is only one type that is the BigDecimal type. Some unconventional notation was chosen, like the <~ and >~ operators for comparison, this is because using the ''=" here would conflict with Scripty pairs which have the form name=value.

If you provide strings, they will be converted to numbers first, it is ok to provide string arguments.

## Reference

**abs**

Absolute value.

**+, -, *, /, ^**

The conventional mathematics operators.

**fin**

Convert the number to a String with 2 decimals as it is used in financial information (invoices and the lot).

**float->int**

Convert to an integer representation.

**zero?**

Test if a number is zero.

**~, >, >~, <, <~**

Numerical comparison. The '~' stands for '=' but the last one cannot be used because it conflicts with Scripty pairs, the Scripty parser will try to create a pair when it encounters a '='.

**number?**

Check if the thing you passed as an argument can be converted to a float.

**rem**

Remainder.

