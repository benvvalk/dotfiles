{ config, pkgs, lib, ... }:

{
  #---------------------------------------------------------------------------
  # kmonad (keyboard remapping)
  #---------------------------------------------------------------------------

  options = {
    kmonad.device = lib.mkOption {
      type = lib.types.str;
      description = "The input device path for kmonad";
    };
  };

  config = {
    services.kmonad = {
      enable = true;
      keyboards.main.device = config.kmonad.device;
      keyboards.main.config = ''
        (defcfg
          input  (device-file "${config.kmonad.device}")
          output (uinput-sink
                      "My KMonad output"
                      "sleep 1 && xset r rate 250 90"
                      )
          fallthrough true
          allow-cmd false
          implicit-around around
        )

        (defsrc
          grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
          tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
          caps a    s    d    f    g    h    j    k    l    ;    '    ret
          lsft z    x    c    v    b    n    m    ,    .    /    rsft
          lctl lmet lalt           spc            ralt rmet cmp  rctl
        )

        (defalias
          a (tap-hold-next-release 200 a lsft)
          e (tap-hold-next-release 200 e lsft)
          s (tap-hold-next-release 200 s lctl)
          d (tap-hold-next-release 200 d lmet)
          f (tap-hold-next-release 200 f lalt)
          j (tap-hold-next-release 200 j ralt)
          k (tap-hold-next-release 200 k rmet)
          l (tap-hold-next-release 200 l rctl)
          ; (tap-hold-next-release 200 ; rsft)
          nav (tap-next esc (layer-toggle nav)))

        (deflayer qwerty
          grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
          tab  q    w    @e   r    t    y    u    i    o    p    [    ]    \
          @nav @a   @s   @d   @f   g    h    @j   @k   @l   @;   '    ret
          lsft z    x    c    v    b    n    m    ,    .    /    rsft
          lctl lmet lalt           spc            ralt rmet cmp  rctl
        )

        (deflayer nav
          _    _    _    _    _    _    _    _    _    _    _    _    _    _
          _    _    _    _    _    _    _    home pgdn pgup end  _    _    _
          _    _    _    _    _    _    _    left down up   rght _    _
          _    _    _    _    _    _    _    _    _    _    _    _
          _    _    _              _              _    _    _    _
        )
      '';
    };
  };

  #---------------------------------------------------------------------------
  # EXWM (Emacs X Window Manager)
  #---------------------------------------------------------------------------

  config = {
    services.xserver.displayManager = {
      # Prefixing the `emacs` start command with `EXWM=1` or `export
      # EXWM=1 &&` doesn't work for some reason, but adding it to
      # `sessionCommands` does.
      sessionCommands = "export EXWM=1";
      session = [
        {
          name = "EXWM";
          # Note: I'm not exactly sure what this setting does, but
          # changing it from "desktop" -> "window" solved a problem
          # with long delays (~ 20 seconds) when running `gpg`/`pass`
          # commands under EXWM. I guess it has something to do with
          # the initial environment setup on login (perhaps starting a
          # DBus session, or setting GNOME environment variables that
          # affect the behaviour of `gnome-keyring-daemon`).
          manage = "window";
          start = ''
            emacs --maximized --debug-init;
            waitPID=$!
         '';
        }
      ];
    };
  };
}