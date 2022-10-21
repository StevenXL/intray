# Intray


## Installation 

### Cloning

Clone the repository

``` shell
git clone https://github.com/NorfairKing/intray.git --recursive
```


### Building

#### With Nix

A flake is provided in `flake.nix`.


#### With stack

Follow the instructions at https://docs.haskellstack.org/en/stable/README/
to install `stack`.
For example:

``` shell
curl -sSL https://get.haskellstack.org/ | sh
```

Then install `autoexporter`:

``` shell
stack install autoexporter
```

Finally, install the intray cli:

``` shell
stack install :intray
```

To also install the other intray applications like the server and web server:

``` shell
stack install
```

### Troubleshooting 

#### Permission Denied (publickey)

If you see an error like this during cloning:

```
Permission Denied (publickey)
```

You probably used ssh-based cloning instead of https-based cloning.
Make sure to use the clone command as shown.

#### Could not execute autoexporter

If you see an error like this during building with stack:

```
intray-data > ghc: could not execute: autoexporter
```

You forgot to run `stack install autoexporter`.

#### Aeson exception

If you see an error like this during building with stacK:

```
Aeson exception:
Error in $.packages[10].completed: failed to parse field 'packages': failed to parse field 'completed': [...]
```

You probably cloned an old version and `git pull`-ed recently.
You still need to remove `stack.yaml.lock`.
You will only need to do this once.


## Configuration

### Config file location

The `intray` cli application looks for config files in these locations by default, in order:

```
- $XDG_CONFIG_HOME/intray/config.yaml
- $HOME/.config/intray/config.yaml
- $HOME/.intray/config.yaml
```

Run `intray --help` to see how intray can be configured.

## Setting up synchronisation with `intray.eu`

Put this in your config file:

```
url: 'https://api.intray.eu'
username: 'YOUR USERNAME HERE'
```

Then register:

``` shell
intray register
```

and login:

``` shell
intray login
```

Now you can sync manually:

``` shell
intray sync
```

Note that syncing will occur automatically any time you change anything locally.
If you would prefer to schedule syncing manually to decrease latency locally, you can use a different syncing strategy:

```
sync: NeverSync
```


### Setting up Intray using Home Manager


See the `homeManagerModules.default` in the provided `flake.nix`.
Within your `home.nix`, add the intray module from this repository:

``` nix
let
  intrayModule = intray.homeManagerModules.${system}.default;
in
{
  imports = [
    intrayModule
  ];
  programs.intray = {
    enable = true;
    sync = {
      enable = true;
      username = "YOUR_USERNAME_HERE";
      password-file = "YOUR_PASSWORD_FILE_HERE";
    };
  };
}
```
### Setting up the Intray servers on NixOs


See the `nixosModules.default` in the provided `flake.nix`.
Within your `configuration.nix`, add the intray module from this repository:

``` nix
let
  intrayModule = intray.nixosModules.${system}.default;
in
{
  imports = [
    intrayModule
  ];
  services.intray.production = {
    enable = true;
    api-server.enable = true;
    web-server.enable = true;
  };
}
```
