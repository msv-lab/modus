# Modusfile Reference

The Modusfile language is a superset of the Dockerfile language. The main difference between Modusfile and Dockerfile is the instruction `RULE` that defines logical rules (Horn clauses). Modus also introduces minor changes: (1) an interpretation of the `FROM` instruction for backward-compatibility, (2) an interpretation of `COPY --from` for parametrised stages, (3) the `QUERY` instruction for specifying default queries.

## Syntax of `RULE`

The syntax of Modus rules is similar to Datalog with Prolog-like extensions. Modus supports integers and strings as primitive types:

- `1` is integer;
- `"foo"` is string.

In Modus, terms are variables, or atoms, or values of primitive types:

- `X` is a variable;
- `bar` is nullary predicate;
- `bar(1, "foo")` is a predication;

Modus also supports compound terms built by applying functors `version` and `image`:

- `bar(version(1, 2, 3, "", ""))` is a predication over a SemVer version. 

Variable names begin with a capital letter or the underscore character `_`.

A rule consists of a head and a body:

```
RULE head :- body
```

A head is an atom or a compound term; a body is a list of atoms or predications:

- `RULE f :- b`
- `RULE f(g, "foo") :- b(t), c`
- `RULE f(X) :- b`
- `RULE f(X, g) :- b(X)`

Rules without bodies are called facts:

- `RULE f(a, b)`

Modus provides special literals for images and versions:

- `i"ubuntu"` is equivalent to `image("", "", "ubuntu", "latest")`;
- `v"1.2.3-alpha"` is equivalent to `version(1, 2, 3, "alpha", "")`.

Modus also provides Prolog-like builtin predicates:

- Unification:
    - ['='/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D)/2) is true if the unification succeeds;
    - ['\\='/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%5C%3D)/2) is true if the unification fails.
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
    - `image/1`: is true for any input, but all the input has to be instantiated.

Modus also provides special syntax for string conversion and substitution:

* a predication `f("${X}")` is transformed into 
```
(string(X), X = _1;
   integer(X), number_string(X, _1);
   atom(X), atom_string(X, _1);
   functor(X, version, 5), version_string(X, _1);
   functor(X, image, 4), image_string(X, _1)),
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

For a given query, Modus solver computes the minimum proof tree in terms of the number of required build actions. This proof tree corresponds to the build tree of the target image. For example, for the following rules

```
RULE base1 :- image(i"ubuntu")
RUN ...  # time-consuming operation

RULE base2 :- image(i"ubuntu")
RUN ...  # time-consuming operation

RULE a :- base1; base2
RULE b :- base2; base1

RULE c :- a, b
```

Modus guarantees that although the dependencies of `a` and `b` will be chosen arbitrarily (because of the disjunction `;`), it will use the same dependency for both of these rules (either both `base1`, or both `base2`) to avoid an extra operation.

Modus solves Horn clauses using SLD resolution, a top-down approach. Compared to Prolog, the semantics of Modus is declarative, i.e. the result does not depend on the order of predicates in the rules.

In Modus, predicates are divided into two categories: image predicates and imageless predicates.

Image predicates represent images. Each image predicate should transitively depend on `image/1`. Rules defining image predicates may be accompanied with build actions, e.g.

```
RULE a :- image(i"alpine")
RUN ...
```

Imageless predicates are those that do not represent any image, and therefore do not depend on `image/1`. Rules defining imageless predicates are not accompanied with build actions. Therefore, imageless predicates are also not used in calculating the cost of the proof tree.

## `FROM`

The `FROM` instruction is supported for backward-compatibility. `FROM` is interpreted as a specific variant of `RULE`. For example, if `base` is a build stage, then

    FROM base AS compile

is interpreted as

    RULE compile :- base

If `base` is not a build stage, then it is interpreted as an image name:

    RULE compile :- i"base"

## `COPY --from`

The semantics of `COPY --from` is extended in such a way that it can refer to a parametrised stage. In this example, `COPY` copies content from the dependency `c(X)`:

```
RULE a(X) :- b(X), c(X)
COPY --from=c ...
```

When several dependencies have the same name, the `as` keyword can be used to define an alias:

```
RULE a(X, Y, Z) :- b(X), b(Y) as c, b(Y)
COPY --from=c ...
```

## `QUERY`

The `QUERY` instruction can be used to define the default query for a Modusfile:

```
RULE a(X, Y, Z) :- ...
...

RULE b(X, V) :- ...
...

QUERY a(X, "foo", 1), b(X, v"1.2") 
```

Several `QUERY` instructions are treated as disjunction.