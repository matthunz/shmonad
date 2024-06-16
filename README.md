# ShMonad

![Demo](https://github.com/matthunz/prompt/blob/main/demo.png?raw=true)

A shell prompt with infinite customization.

## Features
- Extensible: Configure your prompt with a Haskell domain specific language (eDSL)
- High performance: Your prompt runs in parallel with traditional compiled Haskell
- Easy to use: Don't worry about monads if you don't want, just copy a config and get going

## Installation

By default the CLI will follow the [XDG](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) specification
and use `$XDG_CONFIG_HOME` for configuration files.

If the path does not yet exist you can start by creating a `shmonad` directory in your configuration path.
For example:
```sh
mkdir -p ~/.config/shmonad
cd ~/.config/shmonad
```

You can then create a new `config.hs` file, which will be the entrypoint of your configuration.
```hs
import Prompt

main = do
  run $
    path $
      segment Dull Magenta userModule
        <> ( currentDirectoryModule
               >>= \dir -> segment Dull Blue $ textModule $ " \xf07b " ++ dir
           )
        <> ( gitBranchModule
               >>= \branch -> segment Dull Cyan $ textModule $ " \xe725 " ++ branch
           )
```

### Cloning from source
In your `shmonad` configuration directory, you can now clone the latest source code.
```sh
git clone https://github.com/matthunz/shmonad
```

### Building
First create a new [stack](https://docs.haskellstack.org/en/stable/) project in the same configuration directory.
```
stack init
```

Then edit your `stack.yml` to include the `shmonad` source code.
```yml
resolver:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/25.yaml

packages:
  - shmonad
```

Finally, you can install the CLI with:
```
stack install
```

### Adding ShMonad to your shell
In your `.zshrc` add
```sh
eval "$(shmonad init)"
```

## Usage
You can recompile your configuration by running:
```
shmonad recompile
```