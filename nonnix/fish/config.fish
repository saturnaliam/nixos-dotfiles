## --- SETTINGS --- ##
set -g __fish_git_prompt_showdirtystate 1
set fish_greeting

function fish_prompt
	printf ' %s%s%s%s %s><> ' (set_color $fish_color_command) (prompt_pwd) (set_color $fish_color_cwd) (fish_git_prompt) (set_color $fish_color_keyword)
end

## --- ALIASES --- ##
alias rm="rm -i"
alias ls="exa -a --color=always --group-directories-first"
alias ll="exa -al --color=always --group-directories-first"
alias mkdir="mkdir -p"
alias rebuild="sudo nixos-rebuild switch"
alias gaa="git add --all"
alias ga="git add"
alias gm="git commit -m"
alias gp="git push"
alias gl="git pull"
alias gcl="git clone"
alias gch="git checkout"
alias gb="git branch"
alias gs="git status"
