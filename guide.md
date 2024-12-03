Let me break down what goes into creating a programming language in plain terms:

First, you need a Lexer (or Tokenizer):

This is like a person reading text who breaks it into meaningful chunks
For example, when it sees "duke_dennis = 10", it recognizes:

"duke_dennis" as a variable name
"=" as an equals sign
"10" as a number

It's basically turning raw text into labeled pieces

Then you need a Parser:

This takes those chunks and figures out how they relate to each other
It's like understanding grammar in a sentence
It builds what's called an Abstract Syntax Tree (AST)
Think of it like nested boxes: the outer box might be "assignment" which contains two boxes: "variable name" and "value"

After that, you need an Interpreter or Compiler:

An interpreter runs the code directly by walking through that tree
A compiler translates it into another language (like machine code)
For Rotlang, we'll probably want an interpreter first since it's simpler

For our meme language, we might want to start super simple with:

Basic variable assignment
Simple math
The ability to print things
Those funny keywords we saw
