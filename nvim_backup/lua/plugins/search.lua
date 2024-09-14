return {
  {
    {
      "tpope/vim-abolish",
    },
    -- {
    --   -- SEARCH INDEXER
    --   "kevinhwang91/nvim-hlslens",
    --   config = function() require("hlslens").setup() end
    -- },
    {
      -- IMPROVES ASTERISK BEHAVIOR
      "haya14busa/vim-asterisk",
      config = function()
        vim.api.nvim_set_keymap('n', '*',
          [[<Plug>(asterisk-z*)<Cmd>lua require('hlslens').start()<CR>]],
          {})
        vim.api.nvim_set_keymap('n', '#',
          [[<Plug>(asterisk-z#)<Cmd>lua require('hlslens').start()<CR>]],
          {})
        vim.api.nvim_set_keymap('n', 'g*',
          [[<Plug>(asterisk-gz*)<Cmd>lua require('hlslens').start()<CR>]],
          {})
        vim.api.nvim_set_keymap('n', 'g#',
          [[<Plug>(asterisk-gz#)<Cmd>lua require('hlslens').start()<CR>]],
          {})

        vim.api.nvim_set_keymap('x', '*',
          [[<Plug>(asterisk-z*)<Cmd>lua require('hlslens').start()<CR>]],
          {})
        vim.api.nvim_set_keymap('x', '#',
          [[<Plug>(asterisk-z#)<Cmd>lua require('hlslens').start()<CR>]],
          {})
        vim.api.nvim_set_keymap('x', 'g*',
          [[<Plug>(asterisk-gz*)<Cmd>lua require('hlslens').start()<CR>]],
          {})
        vim.api.nvim_set_keymap('x', 'g#',
          [[<Plug>(asterisk-gz#)<Cmd>lua require('hlslens').start()<CR>]],
          {})
      end
    },
    --   {
    --   -- SEARCH AND REPLACE
    --   "nvim-pack/nvim-spectre",
    --   dependencies = { "nvim-lua/plenary.nvim" },
    --   config = function()
    --     require("spectre").setup({
    --       replace_engine = { ["sed"] = { cmd = "gsed" } }
    --     })
    --     vim.keymap.set("n", "<leader>S",
    --       "<Cmd>lua require('spectre').open()<CR>",
    --       { desc = "search and replace" })
    --   end
    -- }
  }
}
