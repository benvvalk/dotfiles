{ config, pkgs, ... }:
{
  # Note: This Cloudflare Tunnel configuration relies on manually
  # creating `credentialsFile` and copying it into
  # `/etc/cloudflared/bd1f1853-2f5a-4d77-901c-258b0db56659`.  It is
  # probably possible to create a more automated setup using
  # `sops-nix`.
  #
  # The tunnel also requires manually configuring some things in the
  # Cloudflare dashboard. IIRC, you basically have to define a mapping
  # from public URL (`dev.sponsoredissues.org`) to the Cloudflare
  # Tunnel ID (`bd1f1853-2f5a-4d77-901c-258b0db56659`).
  services.cloudflared = {
    enable = true;
    tunnels = {
      "bd1f1853-2f5a-4d77-901c-258b0db56659" = {
        credentialsFile = "/etc/cloudflared/bd1f1853-2f5a-4d77-901c-258b0db56659.json";
        default = "http_status:404"; # result when request does not match ingress rule
        ingress = {
          "dev.sponsoredissues.org" = "http://localhost:8000";
        };
      };
    };
  };
}