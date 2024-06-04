{ ... }: {
  programs.starship.enable = true;
  programs.starship.settings = {
    format = "\\[$username@$hostname$directory\\] $git_status$character";

    username = {
      style_root = "white bold";
      style_user = "purple";
      format = "[$user]($style)";
      show_always = true;
    };

    hostname = {
      style = "blue";
      ssh_only = false;
    };

    directory = { format = "[$path]($style)[$read_only]($read_only_style)"; };
  };
}
