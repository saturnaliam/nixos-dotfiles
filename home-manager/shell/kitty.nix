{ config, pkgs, ... }:
{
	programs.kitty = {
		enable = true;
		settings = {
			font_fmaily = "jetbrainsmono nerd font mono";
			font_size = 10;
			enable_audio_bell = false;
			background_opacity = "0.7";
			confirm_os_window_close = 0;
		};
	};
}
