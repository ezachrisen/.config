return {

  -- {
  --   'nvim-telescope/telescope-fzf-native.nvim',
  --   build = 'make',
  -- },

  {
    -- SEARCH
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      --[[
          Opening multiple files doesn't work by default.

          You can either following the implementation detailed here:
          https://github.com/nvim-telescope/telescope.nvim/issues/1048#issuecomment-1220846367

          Or you can have a more complex workflow:
          - Select multiple files using <Tab>
          - Send the selected files to the quickfix window using <C-o>
          - Search the quickfix window (using either :copen or <leader>q)

          NOTE: Scroll the preview window using <C-d> and <C-u>.
        ]]
      local actions = require("telescope.actions")
      local ts = require("telescope")

      ts.setup({
        defaults = {
          layout_strategy = "horizontal",
          -- layout_config = { height = 0.75, preview_height = 0.7 },
          mappings = {
            i = {
              ["<esc>"] = actions.close,
              ["<C-o>"] = actions.send_selected_to_qflist,
            },
          },
          scroll_strategy = "limit",
          file_ignore_patterns = { "node_modules", ".git", "vendor", "api/pb" },
        },
        extensions = { heading = { treesitter = true } },
      })

      ts.load_extension("changed_files")
      ts.load_extension("emoji")
      ts.load_extension("fzf")
      ts.load_extension("heading")
      ts.load_extension("ui-select")
      ts.load_extension("windows")

      vim.g.telescope_changed_files_base_branch = "main"

      -- vim.keymap.set("n", "<leader>fb", "<Cmd>Telescope buffers<CR>", { desc = "search buffers" })
      -- vim.keymap.set("n", "<leader>c", "<Cmd>Telescope colorscheme<CR>", { desc = "search colorschemes" })
      -- vim.keymap.set("n", "<leader>fd", "<Cmd>TodoTelescope<CR>", { desc = "search TODOs" })
      -- vim.keymap.set("n", "<leader>e", "<Cmd>Telescope commands<CR>", { desc = "search Ex commands" })
      -- vim.keymap.set("n", "<leader>ff", "<Cmd>Telescope find_files hidden=true<CR>", { desc = "search files" })
      -- vim.keymap.set("n", "<leader>fg", "<Cmd>Telescope changed_files<CR>", { desc = "search changed files" })
      -- vim.keymap.set("n", "<leader>fr", "<Cmd>Telescope oldfiles<CR>", { desc = "search recent files" })
      -- -- vim.keymap.set("n", "<leader>h", "<Cmd>Telescope help_tags<CR>", { desc = "search help" })
      -- vim.keymap.set("n", "<leader>i", "<Cmd>Telescope builtin<CR>", { desc = "search builtins" })
      -- vim.keymap.set("n", "<leader>j", "<Cmd>Telescope emoji<CR>", { desc = "search emojis" })
      -- vim.keymap.set("n", "<leader>k", "<Cmd>Telescope keymaps<CR>", { desc = "search key mappings" })
      -- vim.keymap.set("n", "<leader>ld", "<Cmd>Telescope diagnostics<CR>", { desc = "search lsp diagnostics" })
      -- vim.keymap.set("n", "<leader>li", "<Cmd>Telescope lsp_incoming_calls<CR>", { desc = "search lsp incoming calls" })
      -- vim.keymap.set("n", "<leader>lo", "<Cmd>Telescope lsp_outgoing_calls<CR>", { desc = "search lsp outgoing calls" })
      -- vim.keymap.set("n", "<leader>lr", "<Cmd>Telescope lsp_references<CR>", { desc = "search lsp code reference" })
      -- vim.keymap.set("n", "<leader>n", "<Cmd>Noice telescope<CR>", { desc = "search messages handled by Noice plugin" })
      -- vim.keymap.set("n", "<leader>fm", "<Cmd>Telescope notify<CR>", { desc = "search messages handled by Noice plugin" })
      -- vim.keymap.set("n", "<leader>q", "<Cmd>Telescope quickfix<CR>", { desc = "search quickfix list" })
      -- vim.keymap.set("n", "<leader>r", "<Cmd>Telescope current_buffer_fuzzy_find<CR>",
      --   { desc = "search current buffer text" })
      -- vim.keymap.set("n", "<leader>s", "<Cmd>Telescope treesitter<CR>", { desc = "search treesitter symbols" }) -- similar to lsp_document_symbols but treesitter doesn't know what a 'struct' is, just that it's a 'type'.
      -- vim.keymap.set("n", "<leader>w", "<Cmd>Telescope windows<CR>", { desc = "search windows" })
      -- vim.keymap.set("n", "<leader>fg", "<Cmd>Telescope live_grep<CR>", { desc = "search text" })
      --
      -- vim.keymap.set("n", "<leader>ls",
      --   "<Cmd>lua require('telescope.builtin').lsp_document_symbols({show_line = true})<CR>",
      -- { desc = "search lsp document tree" })
    end,
  },
  {
    -- FZF SORTER FOR TELESCOPE WRITTEN IN C
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make",
  },
  {
    -- USE TELESCOPE FOR UI ELEMENTS
    "nvim-telescope/telescope-ui-select.nvim",
    config = function()
      require("telescope").setup({})
    end,
  },
  {
    -- SEARCH WINDOWS IN TELESCOPE
    "kyoh86/telescope-windows.nvim",
  },
  {
    -- SEARCH MARKDOWN HEADINGS IN TELESCOPE
    "crispgm/telescope-heading.nvim",
  },
  {
    -- SEARCH EMOJIS IN TELESCOPE
    "xiyaowong/telescope-emoji.nvim",
  },
  {
    -- SEARCH CHANGED GIT FILES IN TELESCOPE
    "axkirillov/telescope-changed-files",
  },
  {
    -- SEARCH NOTES/TODOS IN TELESCOPE
    "folke/todo-comments.nvim",
    dependencies = "nvim-lua/plenary.nvim",
    config = function()
      require("todo-comments").setup({
        keywords = {
          WARN = {
            icon = " ",
            color = "warning",
            alt = { "WARNING", "XXX", "IMPORTANT" },
          },
        },
        search = {
          command = "rg",
          args = {
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--glob=!{node_modules,**/api/vendor/*,**/api/pb/*}",
          },
          pattern = [[\b(KEYWORDS):]],
        },
      })
    end,
  },
}
