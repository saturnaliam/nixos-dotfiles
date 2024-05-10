import socket
import os
import subprocess
from libqtile import bar, layout, qtile, widget, hook
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen
from libqtile.lazy import lazy


base = "#191724"
surface = "#1f1d2e"
overlay = "#26233a"
muted = "#6e6a86"
subtle = "#908caa"
text = "#e0def4"
love = "#eb6f92"
gold = "#f6c177"
rose = "#ebbcba"
pine = "#31748f"
foam = "#9ccfd8"
iris = "#c4a7e7"
highlight_low = "#21202e"
highlight_med = "#403d52"
highlight_high = "#524f67"

mod = "mod4"
terminal = "kitty"

is_desktop = socket.gethostname() == "nixos-desktop"

keys = [
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    Key(
        [mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    Key([mod], "equal", lazy.layout.grow(), desc="Grow window"),
    Key([mod], "minus", lazy.layout.shrink(), desc="Shrink window"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "t", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "c", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod, "shift"],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),
    Key(
        [mod],
        "v",
        lazy.window.toggle_floating(),
        desc="Toggle floating on the focused window",
    ),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "d", lazy.spawn("dmenu_run -h 30"), desc="spawn dmenu"),
    Key([mod], "f", lazy.spawn("firefox"), desc="spawn firefox"),
    Key([mod, "shift"], "e", lazy.spawn("emacsclient -c"), desc="spawn emacs"),
    Key(
        [mod, "shift"], "g", lazy.spawn(f"{terminal} -e tmux attach"), desc="spawn tmux"
    ),
    Key([mod], "g", lazy.spawn(f"{terminal} -e tmux"), desc="spawn new tmux session"),
]

# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )


groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + group number = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + group number = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + group number = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    # layout.Columns(border_focus_stack=["#ffffff", "#ffffff"], border_width=1),
    # layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    layout.MonadTall(border_focus=rose, border_normal=highlight_high, border_width=1),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="Iosevka Nerd Font Propo",
    fontsize=13,
    padding=3,
    background=base,
)
extension_defaults = widget_defaults.copy()


def get_bar(screen_num):
    widgets = [
        widget.GroupBox(
            margin_y=5,
            margin_x=7,
            padding_y=0,
            padding_x=1,
            highlight_method="line",
            highlight_color=highlight_low,
            active=iris,
            foreground=text,
            inactive=highlight_high,
            this_current_screen_border=rose,
            borderwidth=3,
        ),
        widget.TextBox(text="|", foreground=muted),
        widget.WindowName(max_chars=40),
        widget.Spacer(length=8),
        widget.PulseVolume(foreground=foam, fmt=" {}"),
        widget.TextBox(text="|", foreground=muted),
        widget.Battery(
            format="󰁹 {percent:2.0%}", foreground="#a6e3a1", low_foreground=love
        ),
        widget.TextBox(text="|", foreground=muted),
        widget.Wlan(format=" {essid}", interface="wlp2s0", foreground=gold),
        widget.TextBox(text="|", foreground=muted),
        widget.Clock(foreground=iris, format="%H:%M"),
        widget.TextBox(text="|", foreground=muted),
        widget.Mpris2(
            format="{xesam:title} - {xesam:artist}",
            no_metadata_text="Unknown track",
            paused_text="{track}",
            playing_text="{track}",
            width=150,
            foreground=foam,
        ),
        widget.Spacer(length=8),
    ]

    if is_desktop:
        del widgets[4:6]

    if screen_num != 1:
        del widgets[-3:]
    return bar.Bar(widgets, 30)


screens = [
    Screen(top=get_bar(1)),
]

if is_desktop:
    screens.append(Screen(top=get_bar(2)))

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    border_focus=rose,
    border_normal=highlight_high,
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(title="Spotify"),
        Match(wm_class="KeePassXC"),
    ],
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None


@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser("~")
    subprocess.call("/etc/nixos/scripts/autostart-xorg.sh")


# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
