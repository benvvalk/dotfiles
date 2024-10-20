# Note!

I can't symlink these NixOS configuration files into my home directory with
`stow`, because Nix has trouble with flake directories that are symlinks [1].

Instead, I should just edit and apply these files from their original location
in this git repo. For example, since I normally clone this repo to
`~/dotfiles`, I can apply system configuration changes with: 


```
sudo nixos-rebuild switch --flake ~/dotfiles/nixos#nixos
```

Contrary to my expectations, the `#nixos` in the command above is not
interpreted as a comment by `bash`, so it does not need to be quoted.

[1]: https://github.com/NixOS/nix/issues/9253
