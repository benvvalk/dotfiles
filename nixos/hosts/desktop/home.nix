{config, pkgs, inputs, system, ... }: {

    imports = [ ../../common/home.nix ];

    home.packages = with pkgs; [
      gosmee # smee.io client for local webhook testing
      jwt-cli # JSON web token (generate app tokens for GitHub REST API)
      kdePackages.kdenlive # video editor
      obs-studio # video recording software
      pianoteq.standard-trial_8 # MIDI piano simulator (trial version)
      postgresql # for `sponsoredissues.org` development (see note above)
      postgresql.pg_config # for `sponsoredissues.org` development (see note above)
      showmethekey # show keypresses in Wayland (for tree-guide.el video)
    ];

    systemd.user.services.sponsoredissues-django = {
      Unit = {
        Description = "Start local Django web server for sponsoredissues.org";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = "${pkgs.writeShellScript "sponsoredissues-django" ''
          #!/run/current-system/sw/bin/bash
          set -eu -o pipefail
          cd $HOME/git/sponsoredissues.org-dev
          source .envrc
          ./manage.py makemigrations
          ./manage.py migrate
          ./manage.py createcachetable
          ./manage.py collectstatic --no-input
          ./manage.py runserver
        ''}";
      };
    };

    systemd.user.services.sponsoredissues-celery-worker = {
      Unit = {
        Description = "Start local Celery worker for sponsoredissues.org";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = "${pkgs.writeShellScript "sponsoredissues-celery-worker" ''
          #!/run/current-system/sw/bin/bash
          set -eu -o pipefail
          cd $HOME/git/sponsoredissues.org-dev
          source .envrc
          PYTHONUNBUFFERED=1 celery -A sponsoredissues worker
        ''}";
      };
    };

    # Explanation from
    # https://mynixos.com/home-manager/option/home.stateVersion:
    #
    # "It is occasionally necessary for Home Manager to change
    # configuration defaults in a way that is incompatible with
    # stateful data. This could, for example, include switching the
    # default data format or location of a file.
    #
    # The state version indicates which default settings are in effect
    # and will therefore help avoid breaking program
    # configurations. Switching to a higher state version typically
    # requires performing some manual steps, such as data conversion
    # or moving files."
    home.stateVersion = "25.05";
}
