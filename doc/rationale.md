## Rationale

In an ideal world, users should not have to write complex and repetitive code.
They should just specify what they want and let smart programs do the execution for them.

**Problem:** most front end frameworks don't ask their users what they want.
Instead, they ask them to write code that do things.

Using those frameworks, the program has no way to know what the user wants or what he is doing.
Given some code written by the user, it is unable to answer those basic questions:
- Where the data displayed on the screen comes from?
- When does this piece of HTML should be displayed?

Using those frameworks, the program only knows the result of what the user's code is doing:
- It knows the values of those data, but does not know from where they come from.
- It knows what HTML should be displayed but does not know why they should.

The program is not given enough information to do anything on its own,
making it limited in how it can serve the user.

Vrac is specifically designed to let the user say what he wants to do,
so the program can do on its own all the work it could possibly do.
