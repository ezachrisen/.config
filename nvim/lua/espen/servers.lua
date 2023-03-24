local M         = {}

local Terminal  = require('toggleterm.terminal').Terminal

local serverOne = Terminal:new({
  cmd = "./run.sh",
  dir = "~/moonbase/example/server",
  direction = "horizontal",
  close_on_exit = false,
  auto_scroll = true
}
)


local serverTwo = Terminal:new({
  cmd = "ls",
  dir = "git_dir",
  direction = "horizontal",
  close_on_exit = false,
  auto_scroll = true
}
)

function M.StopOne()
  serverOne.send("^C")
end

function M.ToggleServerWindows()
  serverOne:toggle()
  serverTwo:toggle()
end

return M
