{ config, pkgs, ... }:
{
  programs.waybar = {
    enable = true;
    settings = 
      [{
	layer = "top";
	position = "top";
	mod = "dock";
	exclusive = true;
	passthrough = false;
	gtk-layer-shell = true;
	height = 32;

	modules-left = [ "hyprland/workspaces" "clock" ];
	modules-center = [ "hyprland/window" ];
	modules-right = [ "battery" "network" "pulseaudio" ];

	"clock" = {
	  format = "{:󰥔  %I:%M %p   %a, %b %e}"; # format of HH:MM [AM/PM] (DAY), (MONTH), (DATE)
	};

	"network" = {
	  "format-wifi" = "  {essid}";
	};

	"pulseaudio" = {
	  "format" = "  {volume}%";
	};

	"battery" = {
		"format" = "  {capacity}%";
	};

	"hyprland/workspaces" = {
		"persistent-workspaces" = {
			"*" = 5;
		};
	};
      }];

      style = ''
	@define-color bg #1e1e2e;
	@define-color text #cdd6f4;
	@define-color pill #313244;
	@define-color pink #f5c2e7;
	@define-color lavender #b4befe;
	@define-color rosewater #f5e0dc;
	@define-color green #a6e3a1;

      	* {
	  border: none;
	  font-family: "JetBrainsMono Nerd Font", monospace;
	  font-size: 12px;
	}

	window#waybar {
	  background-color: @bg;
	  color: @text;
	  border-radius: 8px;
	  padding: 3px 0px;
	}
	
	#workspaces {
		margin-left: 6px;
	}

	#workspaces button {
		color: @text;
	}

	#workspaces button.active {
		background-color: @pill;
		color: @green;
	}

	#clock,
	#network,
  #pulseaudio,
	#battery {
	  margin: 4px 3px;
	  border-radius: 5px;
	  padding: 0px 15px;
	  background-color: @pill;
	}

	#clock {
	  color: @pink;

	}

	#network {
	  color: @rosewater;
	}

	#pulseaudio {
	  color: @lavender;
	  margin-right: 5px;
	}

	#battery {
		color: @green;
	}
     '';
  };
}
