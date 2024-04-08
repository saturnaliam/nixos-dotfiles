{ config, pkgs, lib, ...  }:
let
mod = "SUPER";
terminal = "kitty";
in {
	home.packages = with pkgs; [
		swaybg
	];

  # fix for obsidian not working on my desktop
  xdg.desktopEntries.obsidian = {
    exec =
      if config.platform == "desktop" then
        "obsidian --enable-unsafe-webgpu --enable-features=Vulkan --use-vulkan=swiftshader --use-webgpu-adapter=swiftshader --no-sandbox %u"
      else
        "obsidian %u";

        name = "Obsidian";
        terminal = false;
        icon = "obsidian";
  };
		
	wayland.windowManager.hyprland = {
		enable = true;
		systemd.enable = true;
		xwayland.enable = true;

		settings = {
			"$mod" = mod;
			"$terminal" = terminal;
			"$menu" = "rofi";

			exec-once = [
				"/etc/nixos/scripts/autostart.sh"
			];

			env = [
				"XCURSOR_SIZE,24"
				"QT_QPA_PLATFORMTHEME_qt5ct"
			];
			
			monitor = [
				"DP-1,1920x1080@240,0x0,1"
				"DVI-D-1,1920x1080@60,1920x0,1"
			];
	
			general = {
				gaps_in = 0;
				gaps_out = 5;
				border_size = 2;
				"col.active_border" = "rgba(f2cdcddd) rgba(f2cdcddd)";
				"col.inactive_border" = "rgba(595959aa)";

				layout = "dwindle";
				
				allow_tearing = false;
			};

			decoration = {
				rounding = 0;
				
				blur = {
					enabled = true;
					size = 6;
					passes = 3;
				};

				drop_shadow = "yes";
				shadow_range = 4;
				shadow_render_power = 3;
				"col.shadow" = "rgba(1a1a1aee)";
			};

			animations = {
				enabled = "yes";

				bezier = [ "myBezier, 0.05, 0.9, 0.1, 1.05" ];

				animation= [
					"windows, 1, 7, myBezier"
					"windowsOut, 1, 7, default, popin 80%"
					"border, 1, 10, default"
					"borderangle, 1, 8, default"
					"fade, 1, 7, default"
					"workspaces, 1, 6, default"
				];
			};

			dwindle = {
				pseudotile = "yes";
				preserve_split = "yes";
			};
			
			master = {
				new_is_master = true;
			};

			gestures = {
				workspace_swipe = "off";
			};

			misc = {
				force_default_wallpaper = 0;
			};

			bind = [
				# hyprland stuff
				"$mod, C, killactive," # kills active window
				"$mod SHIFT, M, exit," # closes hyprland
				"$mod, V, togglefloating" # makes current window float
				"$mod, H, movefocus, r" # moves focus right
				"$mod, J, movefocus, d" # moves focus down
				"$mod, K, movefocus, u" # moves focus up
				"$mod, L, movefocus, l" # moves focus left
				"$mod SHIFT, F, fullscreen" # makes current window fullscreen


				# opening programs
				"$mod, F, exec, firefox" # opens firefox
				"$mod SHIFT, E, exec, emacsclient -c" # opens emacs
				"$mod, D, exec, $menu -show drun" # opens the menu in drun mode
				"$mod, S, exec, $menu -show ssh" # opens the menu in ssh mode
				"$mod, B, exec, $menu -show filebrowser" # opens the menu in filebrowser mode
        "$mod SHIFT, code:61, exec, /etc/nixos/scripts/show-wm-binds.sh" # opens the keybinds help menu
				"$mod, T, exec, $terminal" # opens the terminal
				"$mod, G, exec, $terminal -e tmux" # opens the terminal w/ tmux

				# misc
				"$mod SHIFT, S, exec, grim -g \"$(slurp)\" - | swappy -f -" # makes a screenshot

				# moving workspaces
				"$mod, 1, workspace, 1"
				"$mod, 2, workspace, 2"
				"$mod, 3, workspace, 3"
				"$mod, 4, workspace, 4"
				"$mod, 5, workspace, 5"
				"$mod, 6, workspace, 6"
				"$mod, 7, workspace, 7"
				"$mod, 8, workspace, 8"
				"$mod, 9, workspace, 9"
				"$mod, 0, workspace, 10"
				"$mod SHIFT, 1, movetoworkspace, 1"
				"$mod SHIFT, 2, movetoworkspace, 2"
				"$mod SHIFT, 3, movetoworkspace, 3"
				"$mod SHIFT, 4, movetoworkspace, 4"
				"$mod SHIFT, 5, movetoworkspace, 5"
				"$mod SHIFT, 6, movetoworkspace, 6"
				"$mod SHIFT, 7, movetoworkspace, 7"
				"$mod SHIFT, 8, movetoworkspace, 8"
				"$mod SHIFT, 9, movetoworkspace, 9"
				"$mod SHIFT, 0, movetoworkspace, 10"
			];
		};
	};
}
