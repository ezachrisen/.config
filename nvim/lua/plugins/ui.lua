return {
  { 'anuvyklack/hydra.nvim' },
  {
    -- STATUS LINE
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons", opt = true },
    config = function()
      -- local colors = {
      --   black        = '#282828',
      --   white        = '#ebdbb2',
      --   red          = '#fb4934',
      --   green        = '#b8bb26',
      --   blue         = '#83a598',
      --   yellow       = '#fe8019',
      --   gray         = '#a89984',
      --   darkgray     = '#3c3836',
      --   lightgray    = '#504945',
      --   inactivegray = '#7c6f64',
      -- }
      -- gruvbox = {
      --   normal = {
      --     a = { bg = colors.gray, fg = colors.black, gui = 'bold' },
      --     b = { bg = colors.lightgray, fg = colors.white },
      --     c = { bg = colors.darkgray, fg = colors.gray }
      --   },
      --   insert = {
      --     a = { bg = colors.blue, fg = colors.black, gui = 'bold' },
      --     b = { bg = colors.lightgray, fg = colors.white },
      --     c = { bg = colors.lightgray, fg = colors.white }
      --   },
      --   visual = {
      --     a = { bg = colors.yellow, fg = colors.black, gui = 'bold' },
      --     b = { bg = colors.lightgray, fg = colors.white },
      --     c = { bg = colors.inactivegray, fg = colors.black }
      --   },
      --   replace = {
      --     a = { bg = colors.red, fg = colors.black, gui = 'bold' },
      --     b = { bg = colors.lightgray, fg = colors.white },
      --     c = { bg = colors.black, fg = colors.white }
      --   },
      --   command = {
      --     a = { bg = colors.green, fg = colors.black, gui = 'bold' },
      --     b = { bg = colors.lightgray, fg = colors.white },
      --     c = { bg = colors.inactivegray, fg = colors.black }
      --   },
      --   inactive = {
      --     a = { bg = colors.darkgray, fg = colors.gray, gui = 'bold' },
      --     b = { bg = colors.darkgray, fg = colors.gray },
      --     c = { bg = colors.darkgray, fg = colors.gray }
      --   }
      -- }
      local custom_gruvbox = require 'lualine.themes.gruvbox'
      custom_gruvbox.insert.a.bg = '#fe8019'
      custom_gruvbox.insert.a.fg = '#282828'
      local function current_time()
        return os.date('%I:%M %p', os.time() - (5 * 60 * 60))
      end
      require("lualine").setup({
        options = {
          icons_enabled = true,
          -- theme = "gruvbox-material",
          theme = custom_gruvbox,
          section_separators = { left = '', right = '' },
          component_separators = { left = '', right = '' }
        },
        sections = {
          lualine_a = { 'mode' },
          lualine_c = {
            {
              "filename",
              file_status = true,   -- displays file status (readonly status, modified status)
              path = 1,             -- relative path
              shorting_target = 40, -- Shortens path to leave 40 space in the window
              symbols = {
                unnamed = 'No name',
                newfile = 'New',
              },
            },
          },
          lualine_z = { current_time },
        }
      })
    end
  },
  {
    -- MINIMAP
    "gorbit99/codewindow.nvim",
    config = function()
      require("codewindow").setup({
        auto_enable = false,
        use_treesitter = true, -- disable to lose colours
        exclude_filetypes = {
          "Outline", "neo-tree", "qf", "packer", "help", "noice",
          "Trouble"
        }
      })
      vim.api.nvim_set_keymap("n", "<leader><leader>m",
        "<cmd>lua require('codewindow').toggle_minimap()<CR>",
        {
          noremap = true,
          silent = true,
          desc = "Toggle minimap"
        })
    end
  },
  {
    -- UI IMPROVEMENTS
    "stevearc/dressing.nvim",
    config = function() require("dressing").setup() end
  }, {
  -- NOTE: `:Noice` to open message history + `:Noice telescope` to open message history in Telescope.
  "folke/noice.nvim",
  event = "VimEnter",
  config = function()
    require("noice").setup({
      views = {
        cmdline_popup = {
          title = "Command",
          size = { width = "40%", height = "auto" },
          win_options = {
            winhighlight = {
              Normal = "Normal",
              FloatBorder = "DiagnosticSignInfo",
              IncSearch = "",
              Search = ""
            }
          }
        },
        messages = {
          enabled = false,
        },
        popupmenu = {
          relative = "editor",
          position = { row = 8, col = "50%" },
          size = { width = 100, height = 10 },
          border = { style = "rounded", padding = { 0, 0.5 } },
          win_options = {
            winhighlight = {
              Normal = "Normal",
              FloatBorder = "DiagnosticSignInfo"
            }
          }
        }
      },
      routes = {
        -- skip displaying message that file was written to.
        {
          filter = {
            event = "msg_show",
            kind = "",
            find = "written"
          },
          opts = { skip = true }
        },

        {
          view = "split",
          filter = { event = "msg_show", min_height = 20 },
        },


        {
          filter = {
            event = "msg_show",
            find = "--%d+%%--",
          },
          opts = { skip = true },
        },
        {
          filter = {
            event = "msg_show",
            find = "--No lines in buffer--",
          },
          opts = { skip = true },
        },
        {
          filter = {
            event = "msg_show",
            kind = "search_count",
          },
          opts = { skip = true },
        },
      },
      presets = { long_message_to_split = true, lsp_doc_border = true },
      documentation = {
        opts = {
          win_options = {
            winhighlight = { FloatBorder = "DiagnosticSignInfo" }
          }
        }
      },
      lsp = {
        progress = {
          enabled = false -- I already use fidget configured in ./lsp.lua
        }
      }
    })
  end,
  dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" }
},
  {
    "rcarriga/nvim-notify",
  },
  {
    -- TAB UI IMPROVEMENTS
    "akinsho/bufferline.nvim",
    version = "v3.*",
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function()
      require("bufferline").setup({
        options = { mode = "tabs" },
        highlights = {
          tab = { fg = "#CCCCCC" }
          -- tab_selected = {
          --   fg = "#FF0000"
          -- },
        }
      })
    end
  }, {
  -- FZF USED BY BETTER-QUICKFIX PLUGIN
  "junegunn/fzf",
  build = function() vim.fn["fzf#install"]() end
},
  {
    -- QUICKFIX IMPROVEMENTS
    --
    -- <Tab> to select items.
    -- zn to keep selected items.
    -- zN to filter selected items.
    -- zf to fuzzy search items.
    --
    -- <Ctrl-f> scroll down
    -- <Ctrl-b> scroll up
    "kevinhwang91/nvim-bqf",
    ft = "qf",
    config = function()
      require('bqf').setup({
        preview = {
          wrap = true
        }
      })
    end
  },
  -- }, {
  --   -- WINDOW BAR BREADCRUMBS
  --   "utilyre/barbecue.nvim",
  --   name = "barbecue",
  --   version = "*",
  --   dependencies = {
  --     "neovim/nvim-lspconfig", "SmiteshP/nvim-navic",
  --     "nvim-tree/nvim-web-devicons"
  --   },
  --   config = function()
  --     require("barbecue").setup({
  --       attach_navic = false   -- prevent barbecue from automatically attaching nvim-navic
  --       -- this is so shared LSP attach handler can handle attaching only when LSP running
  --     })
  --   end
  -- }, {
  -- -- SCROLLBAR
  -- "petertriho/nvim-scrollbar",
  -- config = function() require("scrollbar").setup() end

}
