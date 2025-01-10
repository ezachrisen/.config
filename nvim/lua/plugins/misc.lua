return {
  {
    "stevearc/overseer.nvim",
    opts = {},
  },
  {
    "tpope/vim-abolish",
  },
  { "anuvyklack/hydra.nvim" },
  "skywind3000/asyncrun.vim",
  {
    "folke/noice.nvim",
    opts = {
      notify = {
        enabled = false,
      },
    },
  },
  {
    "rcarriga/nvim-notify",
    enabled = false,
  },
  {
    "ellisonleao/gruvbox.nvim",
    lazy = false, -- make sure we load this during startup as it is our main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function()
      require("gruvbox").setup({
        -- undercurl = true,
        -- underline = true,
        -- bold = true,
        -- italic = true,
        -- strikethrough = true,
        -- invert_selection = false,
        -- invert_signs = false,
        -- invert_tabline = false,
        -- invert_intend_guides = false,
        -- inverse = true, -- invert background for search, diffs, statuslines and errors
        -- contrast = "", -- can be "hard", "soft" or empty string
        -- palette_overrides = {},
        dim_inactive = false,
        transparent_mode = false,
        overrides = {
          -- SignColumn = { bg = "#ff9900" }
          Function = { fg = "#83a598", bold = true },
          ["@symbol"] = { fg = "#ebdbb2" },
          -- fmt.[Println]
          ["@method.call"] = { fg = "#d5c4a1" },
          ["@function.call"] = { fg = "#d5c4a1" },
          -- ([x] *q) Call([a] int)
          ["@parameter"] = { fg = "#ebdbb2", bold = true },
          ["@keyword"] = { fg = "#ebdbb2" },
          ["@keyword.function"] = { fg = "#ebdbb2", bold = true },
          ["@keyword.return"] = { fg = "#fb4934" },
          ["@constant.builtin"] = { fg = "#ebdbb2" },
          ["@variable"] = { fg = "#d5c4a1" },
          Operator = { fg = "#d5c4a1", italic = false },
          ["@type"] = { fg = "#fabd2f" },
          ["@type.definition"] = { fg = "#fabd2f", bold = true },
          -- type x struct { [docs] string }
          ["@field"] = { fg = "#d3869b" },
          -- status.[Errorf]
          ["@property"] = { fg = "#d5c4a1" },
        },
      })

      vim.cmd([[colorscheme gruvbox]])
    end,
  },
}
