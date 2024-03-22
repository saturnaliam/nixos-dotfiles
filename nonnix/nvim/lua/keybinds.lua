local wk = require("which-key")
-- TODO add many many many more keybinds
wk.register({
	["<leader>"] = {
		f = {
			name = "file",
			f = { "<cmd>Telescope find_files<cr>", "Find File" },
			r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
			n = { "<cmd>enew<cr>", "New File" },
		},
	}
})
