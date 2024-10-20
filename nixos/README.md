# Note!

I can't symlink these NixOS configuration files into my home directory with
`stow`, because Nix does not support flake directories that are symlinks [1].

Instead, I need to edit and apply these files from their original location in
this git repo. For example, since I normally clone this repo to `~/dotfiles`,
I can apply system configuration changes with: 


```
sudo nixos-rebuild switch --flake './~/dotfiles/nixos#nixos'
```

The leading `./` in the path above is needed so that Nix will intepret it as an
absolute path. Also, the path needs to be quoted, so that `#` is not
interpreted as a comment. 

[1]: https://github.com/NixOS/nix/issues/9253
