{ config, pkgs, ... }:
{
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