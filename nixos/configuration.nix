# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.settings.experimental-features = ["nix-command" "flakes"];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable 3D Acceleration
  services.xserver.videoDrivers = ["amdgpu"];
  hardware.graphics = {
      enable = true;
      enable32Bit = true;
  };

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  services.xserver.displayManager = {
    # Prefixing the `emacs` start command with `EXWM=1` or `export
    # EXWM=1 &&` doesn't work for some reason, but adding it to
    # `sessionCommands` does.
    sessionCommands = "export EXWM=1";
    session = [
       {
         name = "EXWM";
         manage = "desktop";
         start = ''
            emacs --maximized --debug-init;
            waitPID=$!
         '';
       }
    ];
  };

  services.kmonad = {
      enable = true;
      keyboards.dell.device = "/dev/input/by-path/pci-0000:16:00.0-usbv2-0:3.2:1.0-event-kbd";
      keyboards.dell.config = ''
         (defcfg
           ;; For Linux
           input  (device-file "/dev/input/by-path/pci-0000:16:00.0-usbv2-0:3.2:1.0-event-kbd")
           output (uinput-sink
                       "My KMonad output"
                       "sleep 1 && xset r rate 250 90"
                       )

           ;; cmp-seq ralt    ;; Set the compose key to `RightAlt'
           ;; cmp-seq-delay 5 ;; 5ms delay between each compose-key sequence press

           ;; For Windows
           ;; input  (low-level-hook)
           ;; output (send-event-sink)

           ;; For MacOS
           ;; input  (iokit-name "my-keyboard-product-string")
           ;; output (kext)

           ;; Comment this if you want unhandled events not to be emitted
           fallthrough true

           ;; Set this to false to disable any command-execution in KMonad
           allow-cmd false

           ;; Set the implicit around to `around`
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

  # Basic postgres config for local development/testing of
  # `sponsoredissues.org`.
  services.postgresql = {
    enable = true;
    ensureUsers = [
      {
        name = "benv";
        ensureClauses.createdb = true; # grant permission to create new databases (e.g. `createdb` command)
      }
    ];
    # When running `psql` without any options, the default behaviour
    # is to connect to a database with the same name as the current
    # user (e.g. "benv"). It saves some possible
    # confusion/inconvenience if we create this database by
    # default. (It can also be created manually by running `createdb
    # benv`.)
    ensureDatabases = [ "benv" ];
    # Allow passwordless access for local system users, over a Unix
    # domain socket. The `10` argument specifies the priority relative
    # to other overrides (if any). `peer` means that postgres will use
    # a one-to-one mapping between Linux user accounts and postgres
    # user accounts, and use Linux's built-in methods for user
    # authentication.
    authentication = pkgs.lib.mkOverride 10 ''
      # TYPE  DATABASE        USER            ADDRESS         METHOD
      local   all             all                             peer
    '';
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.benv = {
    isNormalUser = true;
    description = "Ben Vandervalk";
    extraGroups = [ "networkmanager" "wheel" "dialout" ];
    packages = with pkgs; [
    #  thunderbird
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     git
     home-manager
     stow
     vim
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}
