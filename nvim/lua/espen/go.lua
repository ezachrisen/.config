local Hydra = require('hydra')
local M = {}

local go_test_tags_string = ""
local go_test_verbose = false
local go_test_run = ""
local go_test_env_string = ""
local go_test_home = ""
local raw_output_flag = false

function M.ToggleQuickFix()
  local qf_exists = false
  for _, win in pairs(vim.fn.getwininfo()) do
    if win["quickfix"] == 1 then
      qf_exists = true
    end
  end
  if qf_exists == true then
    vim.cmd "cclose"
    return
  end
  if not vim.tbl_isempty(vim.fn.getqflist()) then
    vim.cmd "vert 60 copen"
  end
end

function M.TestPackage(dir)
  if (go_test_home ~= "") then
    dir = go_test_home
  end
  print("Running 'go test' in ", dir, "with raw = ", rawOutput())
  vim.cmd(string.format([[
   compiler go
  vertical 30 copen | wincmd L
  lcd %s
  AsyncRun -mode=async -pos=right -cols=40 %s -cwd=%s  -focus=0 %s go test %s %s %s
  ]], dir, rawOutput(), dir, go_test_env_string, testRun(), testVerbose(), testTags()))
end

function testVerbose()
  if (go_test_verbose == false)
  then
    return ""
  end
  return "-v"
end

-- Return go test tags prefixed with -tags
-- If no tags, returns nothing
function rawOutput()
  if (not raw_output_flag)
  then
    return ""
  end
  return "-raw"
end

-- Return go test tags prefixed with -tags
-- If no tags, returns nothing
function testRun()
  if (go_test_run == "")
  then
    return ""
  end
  return "--run=" .. go_test_run
end

-- Return go test tags prefixed with -tags
-- If no tags, returns nothing
function testTags()
  if (go_test_tags_string == "")
  then
    return ""
  end
  return "--tags=" .. go_test_tags_string
end

local flag = function(val)
  if val then
    return ''
  else
    return ''
  end
end


local hint = [[
  ^
  Go test options
  ^
  _v_ %{verbose} verbose      _s_ %{short} short    _o_ %{raw} raw output ^^^
  ^^
  _t_ -tags=%{tags}
  _r_ -run=%{run}
  _e_ env=%{env}
  _h_ test home=%{test_home}
  ^
  _q_
  ^
]]



gohydra = Hydra({
  name = 'Go Options',
  hint = hint,
  config = {
    color = 'amaranth',
    invoke_on_body = true,
    hint = {
      border = 'rounded',
      position = 'middle',
      funcs = {
        tags = function() return go_test_tags_string end,
        run = function() return go_test_run end,
        verbose = function() return flag(go_test_verbose) end,
        raw = function() return flag(raw_output_flag) end,
        short = function() return flag(go_test_short) end,
        env = function() return string.sub(go_test_env_string, 1, 20) end,
        test_home = function() return go_test_home end,
      }
    }
  },
  mode = { 'n', 'x' },
  body = '<Leader>o',
  heads = {
    { 's', function() go_test_short = not go_test_short end,     { desc = 'short' } },
    { 'v', function() go_test_verbose = not go_test_verbose end, { desc = 'raw output' } },
    { 'o', function() raw_output_flag = not raw_output_flag end, { desc = 'verbose' } },
    { 'h', function()
      M.input("Home directory for go test", go_test_home,
        function(val)
          go_test_home = val
        end)
    end, { exit = true, desc = 'set go test home directory' } },
    { 'r', function()
      M.input("go test -run=", go_test_run,
        function(val)
          go_test_run = val
        end)
    end, { exit = true, desc = 'set go run tag' } },
    { 'e', function()
      M.input("environment variables", go_test_env_string,
        function(val)
          go_test_env_string = val
        end)
    end, { exit = true, desc = 'set environment variabls for go test' } },
    { 't', function()
      M.input("go test tags", go_test_tags_string,
        function(val)
          go_test_tags_string = val
        end)
    end, { exit = true, desc = 'set go test tag' } },
    { 'q', nil, { exit = true } }
  }
})

function M.GoMenu()
  gohydra:activate()
end

-- function M.NavigationFloatingWin()
--   -- get the editor's max width and height
--   local width = vim.api.nvim_get_option("columns")
--   local height = vim.api.nvim_get_option("lines")

--   -- create a new, scratch buffer, for fzf
--   local buf = vim.api.nvim_create_buf(false, true)
--   vim.api.nvim_buf_set_option(buf, 'buftype', 'nofile')

--   -- if the editor is big enough
--   if (width > 150 or height > 35) then
--     -- fzf's window height is 3/4 of the max height, but not more than 30
--     local win_height = math.min(math.ceil(height * 3 / 4), 30)
--     local win_width

--     -- if the width is small
--     if (width < 150) then
--       -- just subtract 8 from the editor's width
--       win_width = math.ceil(width - 8)
--     else
--       -- use 90% of the editor's width
--       win_width = math.ceil(width * 0.9)
--     end

--     -- settings for the fzf window
--     local opts = {
--       relative = "editor",
--       width = win_width,
--       height = win_height,
--       row = math.ceil((height - win_height) / 2),
--       col = math.ceil((width - win_width) / 2)
--     }

--     -- create a new floating window, centered in the editor
--     local win = vim.api.nvim_open_win(buf, true, opts)
--   end
-- end

local Input = require("nui.input")
local event = require("nui.utils.autocmd").event


-- function M.getInput(default)
--   local input = Input({
--     position = "50%",
--     size = {
--       width = 60,
--     },
--     border = {
--       style = "single",
--       text = {
--         top = "go test tags",
--         top_align = "center",
--       },
--     },
--     win_options = {
--       winhighlight = "Normal:Normal,FloatBorder:Normal",
--     },
--   }, {
--     prompt = "> ",
--     default_value = go_test_tags_string,
--     on_close = function()
--       print("Input Closed!")
--     end,
--     on_submit = function(value)
--       print("VALUE")
--       go_test_tags_string = value
--     end,
--   })

--   -- function M.input(default,title, fn)
--   --   input.on_submit = fn
--   --   input:mount()
--   -- end

--   -- unmount component when cursor leaves buffer
--   input:on(event.BufLeave, function()
--     input:unmount()
--   end)

-- end

local input = Input({
  position = "50%",
  size = {
    width = 60,
  },
  border = {
    style = "single",
    text = {
      top = "go test tags",
      top_align = "center",
    },
  },
  win_options = {
    winhighlight = "Normal:Normal,FloatBorder:Normal",
  },
}, {
  prompt = "> ",
  default_value = go_test_tags_string,
  on_close = function()
    print("Input Closed!")
  end,
  on_submit = function(value)
    print("VALUE")
    go_test_tags_string = value
  end,
})

function M.input(title, default, fn)
  local input = Input({
    position = "50%",
    size = {
      width = 60,
    },
    border = {
      style = "single",
      text = {
        top = title,
        top_align = "center",
      },
    },
    win_options = {
      winhighlight = "Normal:Normal,FloatBorder:Normal",
    },
  }, {
    prompt = "> ",
    default_value = default,
    on_close = function()
      print("Input Closed!")
    end,
    on_submit = fn
  })
  input:mount()
end

-- unmount component when cursor leaves buffer
input:on(event.BufLeave, function()
  input:unmount()
end)

return M
