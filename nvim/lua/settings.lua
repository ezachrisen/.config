-- OPTIONS
-- To see what an option is set to execute :lua = vim.o.<name>
-- change default copy command to be recursive by default.
vim.g.netrw_localcopydircmd = "cp -r"




vim.o.background = "dark"
vim.o.backup = false
vim.o.clipboard = "unnamedplus"
vim.o.completeopt = "menuone,noinsert,noselect"
vim.o.cursorline = false
vim.o.dictionary = "/usr/share/dict/words"
vim.o.expandtab = true
-- vim.o.grepprg = "rg --vimgrep --multiline-dotall"
vim.o.ignorecase = true
vim.o.inccommand = "split"
-- vim.o.lazyredraw = true (disabled as problematic with Noice plugin)
vim.o.number = true
vim.o.scrolloff = 5
vim.o.shiftwidth = 2
-- vim.o.shortmess = vim.o.shortmess .. "c" -- .. is equivalent to += in vimscript
vim.o.shortmess = "filnxToOc" -- copied default and removed `t` (long paths were being truncated) while adding `c`
vim.o.showmatch = true
-- vim.o.signcolumn = "auto"
vim.o.smartcase = true
vim.o.smartindent = true
vim.o.spell = false
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.swapfile = false
vim.o.tabstop = 2
vim.o.updatetime = 1000 -- affects CursorHold and subsequently things like highlighting Code Actions, and the Noice UI popups.
vim.o.wrap = false
vim.g.incsearch = true
vim.wo.relativenumber = true
--vim.opt.autochdir = true
vim.cmd("set signcolumn=yes:2")
vim.ftNix = false;
-- if vim.fn.has("termguicolors") == 1 then
vim.o.termguicolors = true

--[[
vim.o allows you to set global vim options, but not local buffer vim options.
vim.opt has a more expansive API that can handle local and global vim options.
See :h lua-vim-options
]]
vim.opt.colorcolumn = ""

-- NETRW
--
-- https://vonheikemen.github.io/devlog/tools/using-netrw-vim-builtin-file-explorer/

-- keep the current directory and the browsing directory synced.
-- this helps avoid the "move files" error.
vim.g.netrw_keepdir = 0

-- configure the horizontal split size.
vim.g.netrw_winsize = 30

-- hide the banner (`I` will temporarily display it).
-- vim.g.netrw_banner = 0

-- QUICKFIX

vim.cmd("packadd cfilter")
vim.cmd("set showtabline=0 ")

vim.cmd("vnoremap < <gv")
vim.cmd("vnoremap > >gv")
vim.cmd("setl nolist wrap linebreak")
-- UI

-- LSP UI boxes improvements
--
-- NOTE: Noice plugin will override these settings.
vim.lsp.handlers["textDocument/hover"] =
    vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
vim.lsp.handlers["textDocument/signatureHelp"] =
    vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })
vim.diagnostic.config({ float = { border = "rounded", style = "minimal" } })


local fn = vim.fn

function _G.qftf(info)
  local items
  local ret = {}
  -- The name of item in list is based on the directory of quickfix window.
  -- Change the directory for quickfix window make the name of item shorter.
  -- It's a good opportunity to change current directory in quickfixtextfunc :)
  --
  -- local alterBufnr = fn.bufname('#') -- alternative buffer is the buffer before enter qf window
  -- local root = getRootByAlterBufnr(alterBufnr)
  -- vim.cmd(('noa lcd %s'):format(fn.fnameescape(root)))
  --
  if info.quickfix == 1 then
    items = fn.getqflist({ id = info.id, items = 0 }).items
  else
    items = fn.getloclist(info.winid, { id = info.id, items = 0 }).items
  end
  local limit = 18
  local fnameFmt1, fnameFmt2 = '%-' .. limit .. 's', '…%.' .. (limit - 1) .. 's'
  -- local fnameFmt1, fnameFmt2 = '%limit .. 's', '…%.' .. (limit - 1) .. 's'
  local validFmt = '%s │%5d:%-3d│%s %s'
  -- local validFmt = '%s │%5d:%-3d│%s %s'
  for i = info.start_idx, info.end_idx do
    local e = items[i]
    local fname = ''
    local str
    if e.valid == 1 then
      if e.bufnr > 0 then
        fname = fn.bufname(e.bufnr)
        if fname == '' then
          fname = '[No Name]'
        else
          fname = fname:gsub('^' .. vim.env.HOME, '~')
        end
        -- char in fname may occur more than 1 width, ignore this issue in order to keep performance
        if #fname <= limit then
          fname = fnameFmt1:format(fname)
        else
          fname = fnameFmt2:format(fname:sub(1 - limit))
        end
      end
      local lnum = e.lnum > 99999 and -1 or e.lnum
      local col = e.col > 999 and -1 or e.col
      local qtype = e.type == '' and '' or ' ' .. e.type:sub(1, 1):upper()
      str = validFmt:format(fname, lnum, col, qtype, e.text)
    else
      str = e.text
    end
    table.insert(ret, str)
  end
  return ret
end

vim.o.qftf = '{info -> v:lua._G.qftf(info)}'

-- Adapt fzf's delimiter in nvim-bqf
require('bqf').setup({
  filter = {
    fzf = {
      extra_opts = { '--bind', 'ctrl-o:toggle-all', '--delimiter', '│' }
    }
  }
})


--
--
-- -- Configure the UI aspect of the quickfix window
-- -- NOTE: See https://github.com/kevinhwang91/nvim-bqf#customize-quickfix-window-easter-egg and ~/.config/nvim/syntax/qf.vim
-- local fn = vim.fn
--
-- -- QUICKFIX RESULTS SORTER
-- -- :lua _G.qfSort()
-- function _G.qfSort()
--   local items = fn.getqflist()
--   table.sort(items, function(a, b)
--     if a.bufnr == b.bufnr then
--       if a.lnum == b.lnum then
--         return a.col < b.col
--       else
--         return a.lnum < b.lnum
--       end
--     else
--       return a.bufnr < b.bufnr
--     end
--   end)
--   fn.setqflist(items, 'r')
-- end
--

--     if e.valid == 1 then
--       if e.bufnr > 0 then
--         fname = fn.bufname(e.bufnr)
--         if fname == '' then
--           fname = '[No Name]'
--         else
--           fname = fname:gsub('^' .. vim.env.HOME, '~')
--         end
--         -- char in fname may occur more than 1 width, ignore this issue in order to keep performance
--         if #fname <= limit then
--           fname = fnameFmt1:format(fname)
--         else
--           fname = fnameFmt2:format(fname:sub(1 - limit))
--         end
--       end
--       local lnum = e.lnum > 99999 and -1 or e.lnum
--       local col = e.col > 999 and -1 or e.col
--       local qtype = e.type == '' and '' or ' ' .. e.type:sub(1, 1):upper()
--       str = validFmt:format(fname, lnum, col, qtype, e.text)
--     else
--       str = e.text
--     end
--     table.insert(ret, str)
--   end
--   return ret
-- end
--
-- vim.o.qftf = '{info -> v:lua._G.qftf(info)}'
--
--
vim.api.nvim_set_hl(0, 'SignColumn', { clear }) --- but also see where we load the theme at startup
vim.api.nvim_set_hl(0, 'DiagnosticSignInfo', { clear })
vim.api.nvim_set_hl(0, 'HydraHint', { bg = clear })



-- vim.o.autoread = true
-- vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "CursorHoldI", "FocusGained" }, {
--   command = "if mode() != 'c' | checktime | endif",
--   pattern = { "*" },
-- })
--
--
--


local notify = require("notify")
notify.setup({
  background_colour = "Normal",
  fps = 30,
  icons = {
    DEBUG = "",
    ERROR = "",
    INFO = "",
    TRACE = "✎",
    WARN = ""
  },
  level = 2,
  minimum_width = 50,
  render = "default",
  stages = "fade_in_slide_out",
  timeout = 5000,
  top_down = false
})

vim.notify = require("notify")

require('toggletasks').setup {
  debug = false,
  silent = false,     -- don't show "info" messages
  short_paths = true, -- display relative paths when possible
  -- Paths (without extension) to task configuration files (relative to scanned directory)
  -- All supported extensions will be tested, e.g. '.toggletasks.json', '.toggletasks.yaml'
  search_paths = {
    'toggletasks',
    '.toggletasks',
    '.nvim/toggletasks',
  },
  -- Directories to consider when searching for available tasks for current window
  scan = {
    global_cwd = true,    -- vim.fn.getcwd(-1, -1)
    tab_cwd = true,       -- vim.fn.getcwd(-1, tab)
    win_cwd = true,       -- vim.fn.getcwd(win)
    lsp_root = true,      -- root_dir for first LSP available for the buffer
    dirs = {},            -- explicit list of directories to search or function(win): dirs
    rtp = false,          -- scan directories in &runtimepath
    rtp_ftplugin = false, -- scan in &rtp by filetype, e.g. ftplugin/c/toggletasks.json
  },
  tasks = {},             -- list of global tasks or function(win): tasks
  -- this is basically the "Config format" defined using Lua tables
  -- Language server priorities when selecting lsp_root (default is 0)
  lsp_priorities = {
    ['null-ls'] = -10,
  },
  -- Defaults used when opening task's terminal (see Terminal:new() in toggleterm/terminal.lua)
  toggleterm = {
    close_on_exit = false,
    hidden = true,
  },
  -- Configuration of telescope pickers
  telescope = {
    spawn = {
      open_single = true,   -- auto-open terminal window when spawning a single task
      show_running = false, -- include already running tasks in picker candidates
      -- Replaces default select_* actions to spawn task (and change toggleterm
      -- direction for select horiz/vert/tab)
      mappings = {
        select_float = '<C-f>',
        spawn_smart = '<C-a>', -- all if no entries selected, else use multi-select
        spawn_all = '<M-a>',   -- all visible entries
        spawn_selected = nil,  -- entries selected via multi-select (default <tab>)
      },
    },
    -- Replaces default select_* actions to open task terminal (and change toggleterm
    -- direction for select horiz/vert/tab)
    select = {
      mappings = {
        select_float = '<C-f>',
        open_smart = '<C-a>',
        open_all = '<M-a>',
        open_selected = nil,
        kill_smart = '<C-q>',
        kill_all = '<M-q>',
        kill_selected = nil,
        respawn_smart = '<C-s>',
        respawn_all = '<M-s>',
        respawn_selected = nil,
      },
    },
  },
}
require('telescope').load_extension('toggletasks')
