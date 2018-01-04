# compiler

### lexer ###

```
    ocamllex lexer.mll
    ocamlbuild lexer.native
```
To pipe output into lexer use
```
    echo "foo" | lexer.native
    cat testfile | lexer.native
```
