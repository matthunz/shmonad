![Demo](https://github.com/matthunz/prompt/blob/main/demo.png?raw=true)

## Installation

By default the CLI will follow the [XDG](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) specification
and use `$XDG_CONFIG_HOME` for configuration files.

If the path does not yet exist you can start by creating a `prompt` directory in your configuration path.
For example:
```sh
mkdir -p ~/.config/prompt
cd ~/.config/prompt
```

You can then create a new `config.hs` file, which will be the entrypoint of your configuration.
```hs
import Prompt

main = do
  run
    [ currentDirectoryModule,
      gitBranchModule
    ]
```

### Cloning from source
In your `prompt` configuration directory, you can now clone the latest source code.
```sh
git clone https://github.com/matthunz/prompt
```

### Building
First create a new [stack](https://docs.haskellstack.org/en/stable/) project in the same configuration directory.
```
stack init
```

Then edit your `stack.yml` to include the `prompt` source code.
```yml
resolver:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/25.yaml

packages:
  - prompt
```

Finally, you can install the CLI with:
```
stack install
```
