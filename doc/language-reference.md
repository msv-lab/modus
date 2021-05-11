# Modusfile Reference

The Modusfile language is a superset of the Dockerfile language. The main difference between Modusfile and Dockerfile is the instruction `RULE` that defines logical rules (Horn clauses). Modus also introduces minor changes: (1) an interpretation of the `FROM` instruction for backward-compatibility, (2) an interpretation of `COPY --from` for parametrised stages, (3) the `QUERY` instruction for specifying default queries.

## Syntax of `RULE`

The syntax of Modus rules is similar to Prolog/Datalog. Modus supports integers and strings as primitive types:

- `1` is integer;
- `"foo"` is string.

In Modus, terms are variables, or atoms, or values of primitive types, or compound terms built by applying functors:

- `bar` is nullary predicate;
- `bar(1, "foo")` is an object;
- `bar(1, buz(2))` is a nested object. 

Variables names begin with a capital letter or the underscore character `_`.

A rule consists of a head and a body:

```
RULE head :- body
```

A head is an atom or a compound term; a body is a list of atoms, compound terms or variables:

- `RULE f :- b`
- `RULE f(g, "foo") :- b(t), c`
- `RULE f(X) :- b`
- `RULE f(X, g) :- b(X)`
- `RULE f(X) :- X`

Rules without bodies are called facts:

- `RULE f(a, b)`

Modus provides special literals for images and versions:

- `i"ubuntu"` is equivalent to `image("", "", "ubuntu", "latest")`;
- `v"1.2.3-alpha"` is equivalent to `version(1, 2, 3, "alpha", "")`.

Modus also provides builtin predicates:

- Unification:
    - ['='/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D)/2) is true if the unification succeeds;
    - ['\='/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%5C%3D)/2) is true if the unification fails.
- Comparison of integers and versions:
    - ['<'/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3E)/2)
    - ['>'/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3C)/2)
    - ['=<'/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D%3C)/2)
    - ['>='/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3E%3D)/2)
- Meta-programming
    - [functor/3](https://www.swi-prolog.org/pldoc/doc_for?object=functor/3)
- Type predicates:
    - [integer/1](https://www.swi-prolog.org/pldoc/doc_for?object=integer/1)
    - [string/1](https://www.swi-prolog.org/pldoc/doc_for?object=string/1)
- Type conversions:
    - [number_string/2](https://www.swi-prolog.org/pldoc/doc_for?object=number_string/2)
    - [atom_string/2](https://www.swi-prolog.org/pldoc/doc_for?object=atom_string/2)
    - `version_string/2`
    - `image_string/2`
- String operators:
    - [string_concat/3](https://www.swi-prolog.org/pldoc/man?predicate=string_concat/3)
- Image predicate
    - `image/4`: is true for any inputs, but all inputs have to be instantiated.

Modus also provides special syntax for string conversion and substitution:

* a predication `f("${X}")` is transformed into 
```
(
    string(X), X = _1;
    integer(X), number_string(X, _1);
    atom(X), atom_string(X, _1);
    functor(X, version, 5), version_string(X, _1);
    functor(X, image, 4), image_string(X, _1)
),
f(_1)
```
* a predication `f("a${X}b")` is transformed into 
```
string_concat("a", "${X}", _1), 
string_concat(_1, "b", _2), 
f(_2)
```

Currently, Modus forbids recursive rules.

## Semantics of `RULE`


## `FROM`

The `FROM` instruction is supported for backward-compatibility. `FROM` is interpreted as a specific variant of `RULE`. For example, if `base` is a build stage, then

    FROM base AS compile

is interpreted as

    RULE compile :- base

If `base` is not a build stage, then it is interpreted as an image name:

    RULE compile :- i"base"

## `COPY --from`

## `QUERY`