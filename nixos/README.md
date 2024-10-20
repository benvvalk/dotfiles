# NixOS/home-manager video guide

I created my configuration files and learned about the associated CLI commands
by watching the following video:

"Full NixOS Guide: Everything You Need to Know in One Place!",
https://www.youtube.com/watch?v=nLwbNhSxLd4

# Bootstrapping my config on a new NixOS install

## 1. Enable Nix flakes feature

Add the following line to `/etc/nixos/configuration.nix`:

```
nix.settings.experimental-features = ["nix-command" "flakes"];
```

Then apply the change with:

```
sudo nixos-rebuild switch
```

## 2. Apply my Nix system configuration

Now that Nix flakes are enabled, I am able to apply my own custom
NixOS/home-manager configuration from my `dotfiles` repo.

First clone my `dotfiles` repo to `~/dotfiles`:

```
git clone https://https://github.com/benvvalk/dotfiles.git ~/dotfiles
```

Then apply my custom NixOS configuration with:

```
sudo nixos-rebuild switch ~/dotfiles/nixos#nixos
```

The above command will install the `home-manager` command-line tool, among
other changes.

Note!: Don't try to symlink the files from `~/dotfiles/nixos` into my home
directory, because Nix gets confused by flake directories that are symlinks, as
discussed in "Symlinks to flakes have poor UX":
https://github.com/NixOS/nix/issues/9253 

## 3. Apply my Nix home-manager configuration

```
home-manager switch --flake ~/dotfiles/nixos#benv
```
