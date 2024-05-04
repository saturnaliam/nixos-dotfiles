{ config, pkgs, ... }:
{
	programs.alacritty = {
		enable = true;
		settings = {
			window.opacity = 0.9;

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
