return {
  {
    -- TERMINAL
    "akinsho/toggleterm.nvim",
    version = "v2.*",
    config = function()
      require("toggleterm").setup()

      local Terminal = require('toggleterm.terminal').Terminal
      local htop = Terminal:new({
        cmd = "htop",
        hidden = true,
        direction = "float"
      })

      -- NOTE: This is a global function so it can be called from the below mapping.
      function Htop_toggle() htop:toggle() end

      -- vim.api.nvim_set_keymap("n", "<C-t>",
      --   "<cmd>lua Htop_toggle()<CR>", {
      --   noremap = true,
      --   silent = true,
      --   desc = "toggle htop"
      -- })

      vim.keymap.set("n", "<C-t>",
        "<Cmd>ToggleTerm direction=horizontal size=20<CR>",
        { desc = "toggle floating terminal" })
    end
  }
}
