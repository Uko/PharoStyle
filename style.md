# Pharo Style Guidelines
## Optimization Rules
### Move variable assignment outside of single statement ifTrue:ifFalse: blocks
Moving assignments outside blocks leads to shorter and more efficient code.

### Eliminate unnecessary not's
Eliminate unnecessary not's in relation of conditionals

### Move assignment out of unwind blocks
Move assignment out of unwind blocks.For example[[[[ statements. var := object ] ifCurtailed: block]]][[[ var := [ statements. 	    object] ifCurtailed: block]]]

### Remove assignment has no effect
See my #longDescription .

### #asOrderedCollection/#asArray not needed
A prior conversion to an Array or OrderedCollection is not necessary when adding all elements to a collection.

### keys do: -> keysDo: and valuesDo:
The use of keysDo:/valuesDo: means one intermediate collection created less

### Method just sends super message
Check for methods that just forward the message to its superclass.

### Assignment has no effect
A statement such as x := x has no effect.

### Doesn't use the result of a collect:/select:
Checks for senders of typical collection enumeration methods that return an unused result.

### Uses detect:ifNone: instead of contains:
Checks for the common code fragment: "(aCollection detect: [:each | 'some condition'] ifNone: [nil]) ~= nil".

### Unnecessary "= true"
Check for a =, ==, ~=, or ~~ message being sent to true/false or with true/false as the argument.

### Block immediately evaluated
Check for blocks that are immediately evaluated.

### ifTrue:/ifFalse: returns instead of and:/or:'s
Checks for common ifTrue: returns that could be simplified using a boolean expression.

### Literal array contains only characters
Literal arrays containing only characters can more efficiently represented as strings.

### Temporary variables not read AND written
Checks that all temporary variables are both read and written.

### Uses or's instead of a searching literal
Checks for repeated literal equalitity tests that should rather be implemented as a search in a literal collection.

### Unnecessary size check
Check for code that checks that a collection is non-empty before sending it an iteration message (e.g., do:, collect:, etc.).

### String concatenation instead of streams
Check for code using string concatenation inside some iteration message.

### Unnecessary assignment to a temporary variable
Checks for assignements to temporaries that are not used afterwards.

### Uses "(a and: [b]) and: [c]" instead of "a and: [b and: [c]]"
Checks for inefficient nesting of logical conditions.

### Uses (to:)do: instead of to:do:
Checks for inefficient uses of to:do: that create an unnecessary Interval instance.

### Doesn't use the result of a yourself message
Check for methods sending the yourself message when it is not necessary.

### Check for same statements in ifTrue:ifFalse: blocks
Checks for ifTrue:ifFalse: blocks that have the same code at the beginning or end.

### Check for same statements at end of ifTrue:ifFalse: blocks
Checks for ifTrue:ifFalse: blocks that have the same code at the beginning or end.

### Dead Block
Dead Block. The block is not assigned, not returned and no message is send to it.Often this is a left over from using blocks to comment out code.

## SUnit Rules
### Use assert:equals: instead of assert: and =
Using assert:equals: produces better context on rule failure

### Use assert: instead of should:
should: will be deprecated sooner or leater. Use assert: instead

## Bugs Rules
### Debugging code left in methods
Breakpoints, logging statements, etc. should not be left in production code.

### Method has no timeStamp
For proper versioning, every method should have a timestamp.

### Method source contains linefeeds
Pharo code should not contain linefeed characters.

### Messages sent but not implemented
Checks for messages that are sent by a method, but no class in the system implements such a message. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.

### Sends super new initialize
Checks for methods that wrongly initialize an object twice. Contrary to other Smalltalk implementations Pharo automatically calls #initiailize on object creation.

### References an undeclared variable
Checks for references to a undeclared variables

### Uses True/False instead of true/false
Checks for uses of the classes True and False instead of the objects true and false.

### Variable used, but not defined anywhere
This check is similar to the "References an undeclared variable" check, but it looks for variables that are not defined in the class or in the undeclared dictionary. You probably had to work hard to get your code in this state.

### No direct access of methodDict
nobody should directly access the method dictionary. It is purely an implementation artefact that we use one dictionary and it might change in the future

### Super and Self Messages sent but not implemented
Checks if messages sent to self or super exist in the hierarchy, since these can be statically typed. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.

### Menus missing translations
Literal strings shown to users in menus should be translated.

### Sends unknown message to global
Checks for messages that are sent but not implemented by a global. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.

## Potential Bugs Rules
### Class-side #initialize should not send "super initialize".
Don't send super initialize in class side because some class initialize of class of the top of the hierarchy should not be reinitialized.

### Literal array contains a #true, #false, or #nil but the source doesn't.
#(true false nil) now is equal to {true. false. nil} not {#true. #false. #nil} as it used to be.

### Missing super sends in selected methods.
Checks that methods that should always contain a super message send, actually contain a super message send. For example, the postCopy method should always contain a "super postCopy". The list of methods that should contain super message sends is in #superMessages.

### Returns a boolean and non boolean
This smell arises when a method return a boolean value (true or false) and return some other value such as (nil or self). If the method is suppose to return a boolean, then this signifies that there is one path through the method that might return a non-boolean. If the method doesn''t need to return a boolean, it should be probably rewriten to return some non-boolean value since other programmers reading the method might assume that it returns a boolean.

### Instance variable overridden by temporary variable
Finds methods whose temporary variables override an instance variable. This causes problems if you want to use the instance variable inside the method.

### Temporaries read before written
Checks that all temporaries are assigned before they are used. This can help find possible paths through the code where a variable might be unassigned when it is used.

### Uncommon message send
Sending messages with a common literal (e.g. "Object self") or an uppercase selector name are usually bugs, introduced through missing statement separators.

### Unpackaged code
Code that is not contained in a Monticello package is not versioned and cannot be brought into a different image.

### Assignments on block arguments
Check for assignments on block arguments.For example:[:x :y|	x:= x+y.	]The block argument "x" should not be written

### Unary "accessing" method without explicit return
Checks for any unary "accessing" methods without explicit return statements.

### Overrides a deprecated method
The method overrided a deprecated method. This is a sign that an API has changed in an upstream project and most likely the method should override another one

### Uses A | B = C instead of A | (B = C)
Checks precedence ordering of & and | with equality operators. Since | and & have the same precedence as =, there are common mistakes where parenthesis are missing around the equality operators.

### Empty exception handler
Empty exception handler blocks hide potential bugs. The situation should be handled in a more robust way.

### Doesn't use the result of a =, ~=, etc.
Checks for senders of comparator messages that do not use the result of the comparison.

### Assignment inside unwind blocks should be outside.
Checks assignment to a variable that is the first statement inside the value block that is also used in the unwind block.

### Float equality comparison
Floating point types are imprecise. Using the operators = or ~= might not yield the expected result due to internal rounding differences.

### Modifies collection while iterating over it
Checks for remove:'s of elements inside of collection iteration methods such as do:. 

### Platform dependent user interaction
Check the methods that  use platform dependent user interactions.

### Inspect instances of "A + B * C" might be "A + (B * C)"
Checks for mathematical expressions that might be evaluated different (from left-to-right) than the developer thinks.

### Contains a return in an ensure: block
Checks for return statements within ensure: blocks that can have unintended side-effects.

### Returns value of ifTrue:/ifFalse: without ifFalse:/ifTrue: block
Check for methods that return the value of an ifTrue: or ifFalse: message. These statements return nil when the block is not executed.

### Possible three element point (e.g., x @ y + q @ r)
Checks arithmetic statements for possible three element points (i.e., a point that has another point in its x or y part).

### Unconditional recursion
Checks for unconditional recursion that might cause the image to hang when executed.

### Uses the result of an add: message
Check for possible uses of the result returned by an add: or addAll: messages. These messages return their arguments not the receiver. As a result, may uses of the results are wrong.

## Design Flaws Rules
### Rewrite super messages to self messages
Rewrite super messages to self messages when both refer to same method

### Methods equivalently defined in superclass
Check for methods that are equivalent to their superclass methods.

### Excessive number of arguments
Long argument lists (five or more) can indicate that a new object should be created to wrap the numerous parameters.

### Methods implemented but not sent
Check for methods that are never sent. If a method is not sent, it can be removed.

### Inconsistent method classification
All methods should be put into a protocol (method category) that is equivalent to the one of the superclass, which is a Smalltalk style convention.

### Long methods
Returns all methods that have #longMethodSize number of statements. This check counts statements, not lines.

### Refers to class name instead of "self class"
Checks for direct reference to classes themselves.

### Sends different super message
Checks for methods whose source sends a different super message.

### Utility methods
List methods that have one or more arguments and do no refer to self or an instance variable. These methods might be better defined in some other class or as class methods.

### Sends a deprecated message to a known global
Checks for sends of deprecated messages that might be removed in upcoming releases of Pharo.

## Coding Idiom Violation Rules
### Replace with #allSatisfy:, #anySatisfy: or #noneSatisfy:
Replace ad-hoc implementations (using explicit logic based on do:) of allSatisfy:, anySatisfy: and noneSatisfy: by the adequate calls to #allSatisfy:, #anySatisfy: or #noneSatisfy:. 

### at:ifAbsent: -> at:ifAbsentPut:
Replaces at:ifAbsent: by at:ifAbsentPut:. Its leads to shorter and more readable code.

### "a >= b and: [a <= c]" -> "a between: b and: c"
Replaces "a >= b and: [a <= c]" by "a between: b and: c.

### Use cascaded nextPutAll:'s instead of #, in #nextPutAll:
Use cascaded nextPutAll:'s instead of #, in #nextPutAll:.

### #detect:ifNone: or #contains: -> #anySatisfy:
Replaces detect:ifNone: and contains: by anySatisfy:

### = nil -> isNil AND ~= nil -> notNil
Replaces = nil and == nil by isNil, ~= nil and ~~ nil by notNil to make the code more readable.  

### Replace single branch conditional with guard clause
Transforms single branch conditionals with multi-statement bodies into a sequence of statements guarded by a conditional return.

### Rewrite ifTrue:ifFalse: using min:/max:
The use of the messages #min: and #max: improves code readability and avoids heavily nested conditionals.

### Use "Smalltalk ui theme" and "Smalltalk ui icons"
Do not explicitly refer to UITheme current but use Smalltalk ui theme. Similarly do not directly refer to ThemeIcons current but use Smalltalk ui icons.

### Do not use  `shouldnt: [ ... ] raise: Error` 
Replaces `shouldnt: [ ... ] raise: Error` with  `[ ... ]`

### Use "ifNotEmpty:" not "ifNotEmptyDo:"
ifNotEmptyDo: should not be used as ifNotEmpty: works for blocks with arguments, too.

### Use "ifNotNil:" not "ifNotNilDo:"
ifNotNilDo: should not be used as ifNotNil: works for blocks with arguments, too.

### Use "Smalltalk globals" instead of "Smalltalk"
Do not send requests to "Smalltalk" (which models the whole image)  that are related to the envionment of defines classes and globals

### Sends "questionable" message
Check methods that send messages that perform low level things.

### Literal array contains a #,
Checks for literal arrays that contain the #, symbol. The user may have thought that it was a separator.

### Unnecessary assignment or return in block
Checks ensure:, ifCurtailed:, and showWhile: blocks for assignments or returns that are the last statement in the block. These assignments or returns can be moved outside the block since these messages return the value of the block.

### Sends add:/remove: to external collection
Checks for methods that appear to be modifying a collection that is owned by another object.

### Uses do: instead of collect: or select:'s
Checks for code using the do: method instead of using the collect: or select: methods.

### Uses "size = 0" instead of "isEmpty"
Checks for code using equality tests instead of the message sends.

### Uses do: instead of contains: or detect:'s
Checks for bytecodePrimEqual  using the do: method instead of using the contains: or detect: methods.

### Guarding clauses
Checks for ifTrue: or ifFalse: conditions at end of methods that have two or more statements inside their blocks. Such code might better represent the true meaning of the code if they returned self instead.

### to:do: used instead of collect:
Checks for users of to:do: when the shorter collect: would work.

### Uses to:do: instead of do:, with:do: or timesRepeat:
Checks for use of to:do: when a do:, with:do: or timesRepeat: when should be used.

### to:do: loop also increments a counter
Checks for users of to:do: that also increment or decrement a counter.

### Uses whileTrue: instead of to:do:
Checks for users of whileTrue: when the shorter to:do: would work.

## Style Rules
### Underscore assignements should be avoided
Replace _ by :=

### use uncapitalized instead of withFirstCharacterDownshifted
Use uncapitalized instead of withFirstCharacterDownshifted since withFirstCharacterDownshifted is ugly and is deprecated now.

### Redundant class name in selector
Checks for the class name in a selector. This is redundant since to call the you must already refer to the class name. For example, openHierarchyBrowserFrom: is a redundant name for HierarchyBrowser. Avoiding selector including class name gives a chance to have more polymorphic methods.

### Temporary variable capitalization
Temporary and argument variable names should start with a lowercase letter.

### Unclassified methods
All methods should be put into a protocol (method category) for better readability.

