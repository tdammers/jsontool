# jsontool - A practical tool for wrestling JSON

## INSTALLING

### From Source

- Get `stack` from http://haskellstack.org/
- Unpack or git-clone the source
- cd into the project directory
- Run `stack install` (if stack prompts you to do `stack setup`, do it)

### From Binary Release

No binary releases available yet, t.b.c.

## USAGE

    jsontool [OPTION...] [OPERATION...] [-] [FILE...]

In its simplest form, `jsontool` reads JSON from stdin and outputs it on
stdout, round-tripping it through the JSON engine. Used this way, `jsontool`
acts as a validator that checks for JSON well-formedness and normalizes it to
the most compact form (no excessive whitespace).

Adding the `-pretty` option, `jsontool` becomes a JSON pretty-printer.

Adding operations, it becomes a tool for extracting data from JSON documents.

Options, operations, and query syntax are described in HELP, or just try
`jsontool --help`.

## LICENSE

This is free software, provided to you under the terms of the accompanying
LICENSE.
