# Modusfile Reference

Modusfiles are designed to be similar to Dockerfiles. The differences between Modusfiles and Dockerfiles can be classified into essential and accidental. Essential differences are those that enable higher expressiveness and more powerful semantics. Accidental differences are minor, and mostly are due to the limitations of the current implementation.

## Essential Differences from Dockerfile

### Datalog Syntax

### `RULE` Instruction

### `COPY --from=` Instruction

### `QUERY` Instruction

## Accidental Differences from Dockerfile

`ARG` instructions are currently not supported (https://github.com/mechtaev/modus/issues/9). Stage parameters in Modus have the semantics of `ARG`, but setting them using `docker build --build-arg <varname>=<value>` could lead to unpredictable behavior.

Parser directives are currently noy supported (https://github.com/mechtaev/modus/issues/10).

`--platform` flag for build stages is currently not supported.