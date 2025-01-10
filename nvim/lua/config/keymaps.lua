-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
--
--
vim.keymap.set("n", "<leader>c", function()
  require("espen.go").ToggleQuickFix()
end)
vim.keymap.set("n", "<leader>tp", function()
  require("espen.go").TestPackage(vim.fn.expand("%:p:h"))
end)
vim.keymap.set("n", "<leader>tm", function()
  require("espen.go").GoMenu()
end)
