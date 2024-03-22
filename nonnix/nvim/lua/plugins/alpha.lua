return {
	{ "goolord/alpha-nvim",
		config = function ()
			require"alpha".setup(require"alpha.themes.dashboard".config) -- TODO configure
		end
	}
}
