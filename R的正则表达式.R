字符类
.: 匹配任意单个字符（换行符除外）。
\\d: 匹配任何数字字符，相当于 [0-9]。
\\D: 匹配任何非数字字符，相当于 [^0-9]。
\\w: 匹配任何单词字符（字母、数字、下划线），相当于 [A-Za-z0-9_]。
\\W: 匹配任何非单词字符，相当于 [^A-Za-z0-9_]。
\\s: 匹配任何空白字符（空格、制表符、换行符等），相当于 [ \t\r\n\f\v]。
\\S: 匹配任何非空白字符，相当于 [^ \t\r\n\f\v]。

边界匹配
^: 匹配字符串的起始位置。
$: 匹配字符串的结束位置。
\\b: 匹配单词边界。
\\B: 匹配非单词边界。

数量词
*: 匹配前面的元素零次或多次。
+: 匹配前面的元素一次或多次。
?: 匹配前面的元素零次或一次。
{n}: 精确匹配前面的元素n次。
{n,}: 匹配前面的元素至少n次。
{n,m}: 匹配前面的元素至少n次，至多m次。

分组和选择
(...): 用于分组，匹配括号内的表达式。
|: 或操作符，匹配左右任意一个表达式。

转义字符
\\: 用于转义元字符，使其具有字面意义。例如，\\. 匹配一个点号。

1.匹配数字字符串
# 示例字符串
string <- "123abc456"
# 匹配数字
grepl("\\d+", string)

2.匹配非数字字符串
# 示例字符串
string <- "123abc456"
# 匹配非数字
grepl("\\D+", string)

3.匹配单词字符
# 示例字符串
string <- "abc_123"
# 匹配单词字符
grepl("\\w+", string)

4.匹配非单词字符
# 示例字符串
string <- "abc_123!@#"
# 匹配非单词字符
grepl("\\W+", string)

5.匹配空白字符
# 示例字符串
string <- "Hello World"
# 匹配空白字符
grepl("\\s+", string)

6.匹配字符串的起始位置
# 示例字符串
string <- "Hello World"
# 匹配字符串开头的"H"
grepl("^Hell", string)

7.匹配字符串的结束位置
# 示例字符串
string <- "Hello World"
# 匹配字符串结尾的"d"
grepl("d$", string)

8.匹配单词边界
# 示例字符串
string <- "Hello World"
# 匹配单词"Hello"
grepl("\\bHello\\b", string)

9.匹配零次或多次
# 示例字符串
string <- "aaaab"
# 匹配"a"零次或多次
grepl("a*b", string)

10.匹配一次或多次
# 示例字符串
string <- "aaaab"
# 匹配"a"一次或多次
grepl("a+b", string)

11.匹配零次或一次
# 示例字符串
string <- "ab"
# 匹配"a"零次或一次
grepl("a?b", string)

12.匹配精确次数
# 示例字符串
string <- "aaaab"
# 匹配"a"四次
grepl("a{4}b", string)

13.匹配至少n次
# 示例字符串
string <- "aaaab"
# 匹配"a"至少四次
grepl("a{4,}b", string)

14.匹配至少n次，至多m次
# 示例字符串
string <- "aaaab"
# 匹配"a"至少三次，至多四次
grepl("a{3,4}b", string)

15.使用分组
# 示例字符串
string <- "abcabc"
# 匹配"abc"分组
grepl("(abc)+", string)

16.使用选择
# 示例字符串
string <- "abc123"
# 匹配"abc"或"123"
grepl("abc|123", string)
