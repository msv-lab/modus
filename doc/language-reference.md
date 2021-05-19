# Modusfile Reference

The Modusfile language is a superset of the Dockerfile language. The main difference between Modusfile and Dockerfile is the instruction `RULE` that defines logical rules (Horn clauses). Modus also introduces minor changes: (1) an interpretation of the `FROM` instruction for backward-compatibility, (2) an interpretation of `COPY --from` for parametrised stages, (3) the `QUERY` instruction for specifying default queries.

## Table of Contents

* [RULE](#rule)
    * [Terms](#terms)
    * [Rules](#rules)
    * [Parameter Expansion](#parameter-expansion)
    * [Semantics](#semantics)
* [FROM](#from)
* [COPY \-\-from](#copy---from)
* [QUERY](#query)
* [Technical Details](#technical-details)

## `RULE`

The language of Modus rules is a dialect of Datalog with extensions that help to model the domain of Docker image builds.

### Terms

Modus supports five kinds of terms:

- _Integer_: Represents integer values. For example, `1` and `-20` are integers;
- _String_: Represents arbitrary text. For example, `"foo"` is a string.
- _Variable_: Represents variables that range over concrete terms. Variable names begin with a capital letter or the underscore character `_`. For example, `X` and `_a` are variables;
- _Atom_: Represents unique names (labels) with no inherent meaning. Atom names begin with a small letter. For example, `a` and `bar` are atoms;
- _Compound_: Represents complex objects. Compound terms are built by applying functors `version/5` and `image/4` that represent versions and images respectively.

The `version/5` functor is applied to terms that represent parts of a SemVer version, i.e. 
    
    version(MAJOR: Integer, 
            MINOR: Integer, 
            PATCH: Integer, 
            PRE_RELEASE: String, 
            PATCH: String)

For example,
- `version(1, 2, 3, "alpha", "")` is the SemVer version "1.2.3-alpha";
- `version(1, X, Y, "", "")` is the version number with the major version `1`, while the minor and the major versions are represented with the integer variables `X` and `Y`.

The `image/4` functor is applied to terms that represent parts of a Docker image, i.e.

    image(REGISTRY: String, 
          NAMESPACE: String,
          REPO: String, 
          TAG: String)
          
For example,

- `image("registry.redhat.io", "rhel7", "rhel-atomic", "7.9")` is the image "registry.redhat.io/rhel7/rhel-atomic:7.9"
- `image("", "", "ubuntu", "latest")` is the image "ubuntu:latest"

Modus provides special syntax for images and versions:

- `i"ubuntu"` is equivalent to `image("", "", "ubuntu", "latest")`;
- `v"1.2.3-alpha"` is equivalent to `version(1, 2, 3, "alpha", "")`.

For convenience, the version syntax also supports incomplete SemVer versions:

- `v"1.2"` is equivalent to `version(1, 2, 0, "", "")`.

### Rules

Literals are predicates applied to terms:

- `bar(1, "foo")` is a literal;
- `bar` is nullary predicate.

A rule consists of a head and a body separated with the neck symbol `:-`; the head is a literal; the body is an expression built using literals, the conjunction symbol `,`, and the disjunction symbol `;`:

- `RULE f :- b`
- `RULE f(g, "foo") :- b(t), c`
- `RULE f(X) :- b`
- `RULE f(X, g) :- b(X)`
- `RULE f :- a; b`
- `RULE f(X) :- a, (b; c)`

Rules without bodies are called facts:

- `RULE f(a, b)`

Modus also provides Prolog-like builtin predicates:

- Unification:
    - ['='/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D)/2) is true if the unification succeeds;
    - ['\\='/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%5C%3D)/2) is true if the unification fails.
- Comparison of integers and versions:
    - ['<'/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3E)/2), extended for versions according to [SemVer 2.0.0](https://semver.org/);
    - ['>'/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3C)/2), extended for versions according to [SemVer 2.0.0](https://semver.org/);
    - `'<='/2`, equivalent to ['=<'/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D%3C)/2), extended for versions according to [SemVer 2.0.0](https://semver.org/);
    - ['>='/2](https://www.swi-prolog.org/pldoc/doc_for?object=(%3E%3D)/2), extended for versions according to [SemVer 2.0.0](https://semver.org/).
- Structure inspection:
    - [functor/3](https://www.swi-prolog.org/pldoc/doc_for?object=functor/3)
    - [integer/1](https://www.swi-prolog.org/pldoc/doc_for?object=integer/1)
    - [string/1](https://www.swi-prolog.org/pldoc/doc_for?object=string/1)
- Type conversions:
    - [number_string/2](https://www.swi-prolog.org/pldoc/doc_for?object=number_string/2)
    - [atom_string/2](https://www.swi-prolog.org/pldoc/doc_for?object=atom_string/2)
    - `version_string/2` is a bi-directional conversion between a version and a string. At least one of the two arguments must be instantiated;
    - `image_string/2` is a bi-directional conversion between an image and a string. At least one of the two arguments must be instantiated.
- String operators:
    - [string_concat/3](https://www.swi-prolog.org/pldoc/man?predicate=string_concat/3)
- Image predicate
    - `image/1`: is true for any image object, but the argument has to be instantiated.

Predicates do not throw exceptions like in Prolog. If a value of an incompatible type is passed as an argument, the predicate fails.

### Parameter Expansion

Modus provides special syntax for parameter expansion that allows using variables inside strings via the shell-style `${X}` syntax.

First, parameter expansion can be used as a generic approach to convert values into strings. For example, the literal `f("${X}")` is equivalent to 

```
(string(X), X = _1;
   number_string(X, _1);
   atom_string(X, _1);
   version_string(X, _1);
   image_string(X, _1)),
f(_1)
```

Parameter expansion can also be used to express string concatenation. For example, the literal `f("a${X}b")` is equivalent to

```
string_concat("a", "${X}", _1), 
string_concat(_1, "b", _2), 
f(_2)
```

Parameter expansion can also be used with special syntax for versions and images:

- `v"0.${MINOR}.${MAJOR}"` 
- `i"ubuntu:${TAG}"`

### Semantics

For a given query, the Modus solver computes the minimum proof tree in terms of the number of required build actions. This proof tree corresponds to the build tree of the target image. For example, for the following rules

```
RULE base1 :- image(i"ubuntu")
RUN ...  # time-consuming operation

RULE base2 :- image(i"ubuntu")
RUN ...  # time-consuming operation

RULE a :- base1; base2
RULE b :- base2; base1

RULE c :- a, b
```

Modus guarantees that although the dependencies of `a` and `b` will be chosen arbitrarily (because of the disjunction `;`), it will use the same dependency for both of these rules (either both `base1`, or both `base2`) to avoid an extra operation. For example,

```
c
├── a
│   └── base1
│       └── image(i"ubuntu")
└── b
    └── base1
        └── image(i"ubuntu")    
```

In Modus, predicates are divided into two categories: image predicates and imageless predicates.

Image predicates represent images. Each image predicate should transitively depend on `image/1`. Rules defining image predicates may be accompanied with build actions, e.g.

```
RULE a(X) :- image(i"alpine"),  b(X, Y)
RUN ...
```

All variables appearing in the rule can be used in build stage instruction in the same way as Docker's ARGs. The values corresponding to these variables, say `X` and `Y`, are converted into strings using `"${X}"` and `"${Y}"`.

If there are several image predicates in the body of a rule, the first one is used as the parent image. In the following example, `a` uses `b` as the parent image, because `b` precedes `c` and `f` is an imageless predicate:

```
RULE f(V) :- V > v"1.2"

RULE b :- image(i"alpine")
RULE c :- image(i"ubuntu")

RULE a(V) :- f(V), b, c
```

Imageless predicates are those that do not represent any image, and therefore do not depend on `image/1`. Rules defining imageless predicates are not accompanied with build actions. Therefore, imageless predicates are also not used in calculating the cost of the proof tree.

## `FROM`

The `FROM` instruction is supported for backward-compatibility. `FROM` is interpreted as a special case of `RULE`. For example, if `base` is a build stage, then

    FROM base AS compile

is interpreted as

    RULE compile :- base

If `base` is not a build stage, then it is interpreted as an image name:

    RULE compile :- image(i"base")

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

Several `QUERY` instructions are treated as a disjunction.

## Technical Details

Modus is a dialect of Datalog with the following extensions:

- Ungrounded variables
- Builtin predicates
- Compound terms (currently, only `version` and `image`)

These extensions are chosen because they help in modelling the problem domain. Ungrounded variables allow passing build parameters specified in the query, not in the database. Builtin predicates enable version management and basic string manipulation. Compound terms improve handling of versions and images.

Because of the used extensions, Modus solves Horn clauses using [SLD resolution](https://en.wikipedia.org/wiki/SLD_resolution), a top-down approach. Compared to Prolog that also uses top-down evaluation, the semantics of Modus is more declarative. Specifically, the success of a query does not depend on the order of predicates and rules. Currently, the output of Modus may depend on the order of predicates and rules when are there are several optimal solutions with the same cost.

Currently, Modus does not support recursive rules.