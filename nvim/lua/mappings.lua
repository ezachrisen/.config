vim.keymap.set("", "<leader><leader>ps", "<Cmd>PackerSync<CR>", { desc = "update vim plugins" })
vim.keymap.set("", "<leader><leader>pc", ":PackerCompile<CR>:echo 'PackerCompile complete'<CR>",
  { desc = "packer compile" })

vim.keymap.set("", "±", "<Cmd>nohlsearch<CR>", { desc = "turn off search highlight" })
vim.keymap.set("", "<leader>c", "<Cmd>cclose<CR>", { desc = "close the quickfix window" })

vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "scroll down and then center the cursorline" })
vim.keymap.set("n", "<leader>p", function() require 'telescope'.extensions.projects.projects {} end)
vim.keymap.set("n", "<leader><leader>n", "<Cmd>set norelativenumber number<CR>")
vim.keymap.set("n", "<leader><leader>o", "<Cmd>set norelativenumber nonumber<CR>")
vim.keymap.set("n", "<leader>h", "<Cmd>nohlsearch<cr>")
-- vim.keymap.set("n", "<leader>tp", "<Cmd>GoTestPkg<cr>")
-- vim.keymap.set("n", "<leader>tm", "<Cmd>GoTest<cr>")
vim.keymap.set("n", "<leader><leader>r", "<Cmd>set relativenumber<CR>")
vim.keymap.set("n", "<C-c>s", "<Cmd>set mouse=nvi<CR>")
vim.keymap.set("n", "<C-c>k", "<Cmd>set mouse=<CR>")
vim.keymap.set("n", "<leader>m", "<Cmd>make<CR>", { desc = "run make" })
vim.keymap.set("n", "<leader>cl", "<Cmd>colorscheme leuven<CR>", { desc = "leuven" })

-- file opening commands
vim.keymap.set("n", "<leader>cd", "<Cmd>cd %:p:h<CR>", { desc = "open file in folder relative to current file" })

vim.keymap.set("n", "<leader>cg", "<Cmd>colorscheme gruvbox<CR><Cmd>set background=dark<CR>")
vim.keymap.set("n", "<leader>cm",
  function()
    vim.cmd("colorscheme material")
    require('material.functions').change_style('lighter')
  end
  , { desc = "material-light" })

function _G.set_terminal_keymaps()
  local opts = { buffer = 0 }
  vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], opts)
  vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
  vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
  vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
  vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
end

vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')
