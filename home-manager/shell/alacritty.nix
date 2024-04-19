{ config, pkgs, ... }:
{
	programs.alacritty = {
		enable = true;
		settings = {
			window.opacity = 0.9;
			
			colors.primary = {
				foreground = "#e0def4";
				background = "#232136";
			};

			colors.selection = {
				text = "#232136";
				background = "#e0def4";
			};

			colors.normal = {
					black = "#e0def4";
					red = "#eb6f92";
					green = "#3e8fb0";
					yellow = "#f6c177";
					blue = "#9ccfd8";
					magenta = "#c4a7e7";
					cyan = "#ea9a97";
					white = "#e0def4";
			};
			
			colors.bright = {
					black = "#e0def4";
					red = "#eb6f92";
					green = "#3e8fb0";
					yellow = "#f6c177";
					blue = "#9ccfd8";
					magenta = "#c4a7e7";
					cyan = "#ea9a97";
					white = "#e0def4";
			};

			font =
				let family = "JetBrainsMono Nerd Font";
					font-style = style: { inherit family style; };
				in {
					normal = font-style "Regular";
					bold = font-style "Bold";
					italic = font-style "Italic";
					bold_italic = font-style "Bold Italic";

					size = 8;
				};
		};
	};
}
