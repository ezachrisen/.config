local M         = {}

local Terminal  = require('toggleterm.terminal').Terminal

local serverOne = Terminal:new({
  cmd = "go run *.go",
  dir = "~/code/mars/example/api",
  direction = "vertical",
  close_on_exit = false,
  auto_scroll = true
}
)


local serverTwo = Terminal:new({
  cmd = "go run *.go",
  dir = "~/code/mars/example/ui",
  direction = "vertical",
  close_on_exit = false,
  auto_scroll = true
}
)

function M.StopOne()
  serverOne.send("^C")
end

function M.ToggleServerWindows()
  serverOne:toggle()
  vim.wait(3000, function()
  end)
  serverTwo:toggle()
end

return M
