{ ... }:
{
	imports = [ ./shell ./wayland ];

	home.username = "lucia";
	home.homeDirectory = "/home/lucia";
	home.stateVersion = "23.11";

	programs.git = {
		enable = true;
		userName = "saturnaliam";
		userEmail = "luciacdev@gmail.com";
		
		extraConfig = {
			init.defaultBranch = "main";
			commit.gpgsign = true;
			gpg.format = "ssh";
			user.signingkey = "~/.ssh/github_signing.pub";
			safe.directory = [ "/etc/nixos" ];
		};
	};
}
