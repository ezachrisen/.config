local Hydra = require('hydra')
local M = {}

local go_test_tags_string = "spanner,firestore,integration"
local go_test_verbose = false
local go_test_run = ""
local go_test_env_string = ""

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
  vim.cmd(string.format([[
 vert 60 copen
 lcd %s
 AsyncRun -mode=async -pos=right -cols=80 -cwd=%s -focus=0 %s go test %s %s
 ]], dir, dir, go_test_env_string, testVerbose(), testTags()))
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
function testTags()
  if (go_test_tags_string == "")
  then
    return ""
  end
  return "--tags=" .. go_test_tags_string
end

-- echo bufnr('%')
-- local outputbufnr = 27
-- local command = {"go", "test", "-v", "./..." }

-- local test_command = function(dir)
--   local val = "cd " .. dir .. " && go test -v ./... "
--   -- local val={"cd ", dir, "go" , "test", "-v", "./..."}
--   return val
-- end


--   vim.api.nvim_create_autocmd("BufWritePost", {
--       group = vim.api.nvim_create_augroup("ezgo", {clear = true}),
--       pattern = pattern,
--       callback = function()
--         local lines = {}
--         local bufdir = vim.fn.expand('%:p:h')
--        -- bufdir = "/home/espen.zachrisen/maui/api"
--           vim.api.nvim_buf_set_lines(outputbufnr, 0, -1, false, {bufdir})

--           local scroll_to_bottom = function(bufnr)
--             local winid = vim.fn.bufwinid(bufnr)
--             local lines = vim.api.nvim_buf_line_count(outputbufnr)
--             vim.api.nvim_win_set_cursor(winid,{lines,0})
--           end

--           local append_data = function(_, data)
--             if data then
--               vim.api.nvim_buf_set_lines(outputbufnr, -1,-1, false, data)
--               scroll_to_bottom(outputbufnr)
--             end
--           end


--         local finish = function()
--           vim.api.nvim_buf_set_lines(outputbufnr, -1, -1, false, {"Done!"})
--           scroll_to_bottom(outputbufnr)
--           vim.fn.setqflist({}, 'r', {
--             title = "SOMETHING",
--             lines = lines,
--             efm = "%f:%l: ",
--           })
--           vim.cmd([[doautocmd QuickFixCmdPost]])
--         end



--           -- vim.api.nvim_buf_set_lines(outputbufnr, 0, -1, false, {"go test output:"})
--           vim.fn.jobstart(test_command(bufdir), {
--               stderr_buffered = false,
--               stdout_buffered = false,
--               on_stdout = append_data,
--               on_stderr = append_data,
--               on_:exit = finish,
--             })
--           end,
--         })
--       end


-- attach_to_buffer(outputbufnr, "*.go", {"go", "test", "-v", "./..."})



local flag = function(val)
  if val then
    return '[x]'
  else
    return '[ ]'
  end
end


local hint = [[
  ^ ^        Options
  ^
  _v_ %{verbose} verbose
  _s_ %{short} short
  _t_ -tags=%{tags}
  _r_ -run=%{run}
  _e_ env=%{env}
  ^
       ^^^^                _<Esc>_
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
        short = function() return flag(go_test_short) end,
        env = function() return go_test_env_string end,
      }
    }
  },
  mode = { 'n', 'x' },
  body = '<Leader>o',
  heads = {
    { 's', function() go_test_short = not go_test_short end,     { desc = 'short' } },
    { 'v', function() go_test_verbose = not go_test_verbose end, { desc = 'verbose' } },
    { 'r', function()
      M.input("go test -run=", go_test_run,
        function(val)
          print("FINISHED", val)
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
    { '<Esc>', nil, { exit = true } }
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
