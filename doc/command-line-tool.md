# Command Line Tool

Modus provide the command line tool `modus-transpile` that coverts a given Modusfile to a Dockerfile that builds a given query. The query is specified using the `QUERY` instruction, or can be specified/overridden using the `--query` option. `modus-transpile` is intended to be used in conjunction with the `docker build` command:

    $ modus-transpile Modusfile --query compile > Dockerfile
    $ docker build .

The same can be expressed as a single command using [process substitution](https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html):

    $ docker build . -f <(modus-transpile Modusfile --query compile)
    
`modus-transpile` can also output the proof of a given query that is used for Dockerfile generation:

    $ modus-transpile Modusfile --query compile --proof

By default, `modus-transpile` outputs Dockerfile instructions on the standard output, but can also print them into the file specified using the `--output` option.

`modus-transpile` supports multi-file builds using the `--include` option followed by a comma-separated list of files:

    $ modus-transpile Modusfile --include lib1/Modusfile,lib2/Modusfile --query compile > Dockerfile


