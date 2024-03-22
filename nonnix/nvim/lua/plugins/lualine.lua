return {
	{ "nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {
			sections = {
				lualine_a = { "filename" },
				lualine_b = { "progress" },
				lualine_c = { "diagnostics" },
				lualine_x = { "branch" },
				lualine_y = { "filetype" },
				lualine_z = { "mode" },
			}
		},
	}
}
