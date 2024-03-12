{ config, pkgs, ... }:
{
	programs.alacritty = {
		enable = true;
		settings = {
			window.opacity = 0.5;
			
			colors.primary = {
				foreground = "#cdd6f4";
				background = "#1e1e2e";
			};

			colors.selection = {
				text = "#1e1e2e";
				background = "#f5e0dc";
			};

			colors.normal = {
					black = "#45475a";
					red = "#f38ba8";
					green = "#a6e3a1";
					yellow = "#f9e2af";
					blue = "#89b4fa";
					magenta = "#f5c2e7";
					cyan = "#94e2d5";
					white = "#bac2de";
			};
			
			colors.bright = {
				black = "#45475a";
				red = "#f38ba8";
				green = "#a6e3a1";
				yellow = "#f9e2af";
				blue = "#89b4fa";
				magenta = "#f5c2e7";
				cyan = "#94e2d5";
				white = "#bac2de";
			};

			font =
				let family = "JetBrainsMono";
					font-style = style: { inherit family style; };
				in {
					normal = font-style "Regular";
					bold = font-style "Bold";
					italic = font-style "Italic";
					bold_italic = font-style "Bold Italic";

					size = 12;
				};
		};
	};
}
