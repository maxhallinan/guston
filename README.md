# wiz

A dialect of Lisp.

**Grammar**

```ebnf
sexpr = <atom> | "(" , { sexpr } , ")"

atom = symbol

symbol = symbol-start , { symbol-char }

symbol-initial = "!" | "$" | "%" | "&" | "*" | "/" | ":" | "<" | "=" | ">" | "?"
               | "~" | "_" | "^" | letter

symbol-subsequent = symbol-initial | digit | "." | "+" | "-"
```
