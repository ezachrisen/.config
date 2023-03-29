return {
  {
    -- MAKE DOT OPERATOR WORK IN A SENSIBLE WAY
    "tpope/vim-repeat"
  }, {
  -- SWAPPABLE ARGUMENTS AND LIST ELEMENTS
  "mizlan/iswap.nvim",
  config = function() require("iswap").setup() end
}, {
  -- BLOCK SORTER
  "chiedo/vim-sort-blocks-by"
}, {
  -- MODIFY SURROUNDING CHARACTERS
  "kylechui/nvim-surround",
  config = function() require("nvim-surround").setup() end
}, {
  -- CODE COMMENTS
  "numToStr/Comment.nvim",
  config = function()
    require("Comment").setup()

    vim.keymap.set("n", "<leader><leader><leader>", "<Cmd>norm gcc<CR>",
      { desc = "comment a single line" })
    vim.keymap.set("v", "<leader><leader><leader>",
      "<Plug>(comment_toggle_linewise_visual)",
      { desc = "comment multiple lines" })
  end
}, {
  -- DISPLAY HEX COLOURS
  "norcalli/nvim-colorizer.lua",
  config = function() require("colorizer").setup() end
}, {
  -- GENERATE HEX COLOURS
  "uga-rosa/ccc.nvim"
},
  -- {
  --   "folke/which-key.nvim",
  --   config = function()
  --     vim.o.timeout = true
  --     vim.o.timeoutlen = 300
  --     require("which-key").setup({
  --       mode = "n", -- NORMAL mode
  --       -- your configuration comes here
  --       -- or leave it empty to use the default settings
  --       -- refer to the configuration section below
  --     })
  --   end,
  -- },
  {
    'junegunn/vim-easy-align'
  }
}
