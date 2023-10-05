# MinBasic guide

## Keywords and case sensitivity

Most things in MinBasic are described with keywords.
These keywords are *not* case-sensitive, meaning that `if x > 0 then` is equivalent to `IF x > 0 THEN`.

User-defined things, like functions and variables, *are case-sensitive*. So `myVariable`, `MYVARIABLE` and `myvariable` all refer to different things.

We recommend to prefer uppercase for keywords and lowercase for variable names.

## Comments

Comments are prefixed with the `REM` keyword.

## Variables and assignments

<!-- TODO: prevent usage of LET in incorrect places -->

To assign a value to a variable, use the `variable = value` syntax, where `value` can be any expression.
Optionally, you can prefix the assignment with the `LET` keyword.

To use a variable, simply put its name in an expression.

*Note: multi-line expressions are not yet supported.*

Variables don't need to be declared, and values can be assigned to them at any point in time.
Using a variable before it was assigned any value will yield `null` instead.

### Examples

```basic
REM Sets the variable "answer" to 42:
answer = 42

REM Also sets the variable "answer" to 42:
LET answer = 42

REM Sets the variable "x" to 21:
x = answer / 2

REM Increments "x" by one:
x = x + 1
```


## Expressions

The following binary operators are supported:

- Addition: `a + b`
- Subtraction: `a - b`
- Multiplication: `a * b`
- Division: `a / b`
- Modulo: `a % b`
- Less than: `a < b`
- Greater than: `a > b`
- Less than or equal: `a <= b`
- Greater than or equal: `a >= b`
- Equal: `a == b`
- Not equal: `a != b`

Multiplication and division have a greater precedence than addition and subtraction.
Comparisons have the lowest precedence.
You can wrap sub-expressions in parentheses to override precedence.

Some additional operators are only available by calling builtin functions, which are case-insensitive:

- Maximum: `MAX(a, b)`
- Minimum: `MIN(a, b)`
- Square root: `SQRT(a)`
- Floor: `FLOOR(a)`
- Ceil: `CEIL(a)`
- Round: `ROUND(a)`
- Rand: `RAND(a)`, generates a random number between `0` and `a`

### Examples

```basic
REM Picks a random integer between 0 and 63
n = FLOOR(RAND(64))

REM Sets x to the remainder of n by 8, and y by the integer part of n / 8
x = n % 8
y = FLOOR(n / 8)

REM Sets dist to the euclidean distance between (0, 0) and (x, y)
dist = SQRT(x * x + y * y)
```

## Jumps and labels

Jumping allows you to interrupt the regular flow of instruction to go to another point in the program.

To perform a jump, you will first need to define where you want to jump to.
You have two options: prefixing a line with a number, or writing a named label.

Then, use the `GOTO` statement to jump to either a line number, or a label.

### Using line numbers

```basic
REM The following lines have been numbered. The numbers chosen are arbitrary, but they are commonly increasing multiples of 10,
REM which allows you to squeeze in debugging statements when needed.
10 PRINT "Hello, world"
20 PRINT "This is line 20"

REM We then jump back to line 20, which will cause an infinite loop printing "This is line 20"
30 GOTO 20
```

### Using labels

```basic
REM We define here the "start" label
start:
PRINT "Hello, world"

REM We then jump to the "start" label, causing an infinite loop printing "Hello, world"
GOTO start
```

## Conditions

The `IF` keyword allows you to execute different parts of the code depending on whether a condition is met or not.
The syntax for `IF` is as follows:

```basic
IF condition THEN
    REM Code to be executed if "condition" is true
ELSE
    REM Code to be executed if "condition" is false
END IF
```

If you do not need to execute code when the condition is false, then you can omit the `ELSE` keyword.

### Example

```basic
REM This is a condition without an ELSE block:
IF age < 0 THEN
    PRINT "It seems like you weren't born yet..."
END IF

IF age < 18 THEN
    PRINT "You are underaged"
ELSE
    REM We can nest conditions within other conditions:
    IF age == 18 THEN
        PRINT "You just turned 18!"
    ELSE
        PRINT "You're over 18"
    END IF
END IF
```

## Loops

MinBasic offers multiple ways to execute a block of code multiple times, on top of manually jumping to an earlier point in the code:

### `FOR` loops

`FOR` loops allow you to run a piece of code for a fixed amount of iterations, incrementing a variable when doing so.
The syntax is as follows:

```basic
REM Prints the numbers from 1 to 10, with 10 included
FOR x = 1 TO 10
    PRINT x
NEXT x
```

The `FOR` keyword expects a variable name (here `x`), an initial value (here `1`), a maximal value (here `10`), and optionally an increment, which defaults to `1`.
To specify the increment, append `STEP n` to the `FOR` instruction: `FOR x = 1 TO 10 STEP 2`.

The loop body is then executed, until the `NEXT` statement is reached, telling the loop to jump to the beginning, increment the variable, compare it and possibly execute the loop body.

If the initial value is bigger than the maximal value, then the loop body will not be executed.

### `WHILE` loops

`WHILE` loops allow you to execute a piece of code any amount of time, until a condition turns false.
The syntax is as follows:

```basic
REM Divides x until it is an odd number
WHILE x % 2 == 0
    x = x / 2
WEND
```

If the condition (here `x % 2 == 0`) yields false on the first iteration, then the loop body will not be executed.

`WEND` may be replaced with `END WHILE`, similar to VisualBasic.

### `DO WHILE` loops

Much like `WHILE` loops, `DO WHILE` loops execute a piece of code until a condition turns false.
The difference, however, is that the loop body will be executed at least once:

```basic
REM Will repeatedly set x to 3*x+1, until it becomes an even number
DO WHILE x % 2 == 1
    x = 3*x + 1
LOOP
```

## Subroutines

Subroutines allow you to jump to a point in your code, and return back to where you entered the subroutine.
This allows you to re-use a piece of code, without duplicating it.

```basic
color = "white"
amount = 0
GOSUB display

WHILE true
    READ(amount, cell1, 0)
    IF amount > 0 THEN
        color = "green"
        GOSUB display
    ELSE
        color = "red"
        GOSUB display
    END IF
WEND

display:
    PRINT "Amount: [", color, "]", amount, "[white]"
    PRINT_FLUSH(message1)
    RETURN
```

As of now, subroutines save their return point in a numeric variable, using modulus arithmetic to pack many returns in one.
This allows for 17 nested subroutine calls, before the "return stack" overflows.
In most program however, this limitation shouldn't cause issues, unless if you forgot a `RETURN` or cause an infinite amount of recursive `GOSUB`s.

The "return stack" is cleared at the beginning of the generated program.

## Interacting with the world

`mlog` provides several ways for processors to interact with the in-game world, which are reflected in MinBasic using functions:

<!-- ### `sensor`

The `sensor` instruction becomes the `.` operator:

```basic
health = SENSOR(@unit, @health)
health = @unit.health

@unit.health = @unit.health - 10
``` -->
