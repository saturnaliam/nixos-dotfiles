{ config, pkgs, ... }:
{
  programs.waybar = {
    enable = true;
    settings =
      [{
        layer = "top";
        position = "top";
        mod = "dock";
        exclusive = "true";
        passthrough = false;
        gtk-layer-shell = true;
        height = 32;

        modules-left = [ "hyprland/workspaces" "hyprland/window" ];
        modules-center = [  ];
        modules-right = [ "battery" "clock" "network" "pulseaudio" ];

        "clock" = {
          format = "{:%I:%M}   |   ";
        };

        "network" = {
          "format-wifi" = "{essid}   |   ";
        };

        "pulseaudio" = {
          "format" = "{volume}%   ";
        };

        "battery" = {
          "format" = "{capacity}%   |   ";
        };
      }];

    style = ''
      * {
        border: none;
        font-size: 10px;
      }

      window#waybar {
        background-color: #000000;
        color: #ffffff;
      }

       #workspaces button {
           color: #ffffff;
       }
    '';
  };
}
