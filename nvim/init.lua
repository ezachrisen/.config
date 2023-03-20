vim.opt.termguicolors = true
vim.g.mapleader = ','
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git", "--branch=stable", -- latest stable release
    lazypath
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup("plugins")
require("autocommands")
require("mappings")
require("settings")
require("espen.go")


vim.keymap.set('n', "<leader>tt", function() require("espen.go").ToggleQuickFix() end)
vim.keymap.set('n', '<leader>tp', function() require("espen.go").TestPackage(vim.fn.expand('%:p:h')) end)
vim.keymap.set('n', '<leader>o', function() require("espen.go").GoMenu() end)
