# Grammar Exchanger

* Eliminate direct/indirect left recursion
* Print like a scala code

Before
```
AmbiguousName:
Identifier
AmbiguousName "." Identifier
```
After
```
AmbiguousName:
Identifier [AmbiguousNameSuf]

AmbiguousNameSuf:
"." Identifier [AmbiguousNameSuf]
```
## Usage
wait...