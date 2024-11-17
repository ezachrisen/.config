function promptMake()
  vim.ui.input({
    prompt = "Enter a value: ",
    default = "default value",
    completion = "file",
    highlight = function(input)
      if string.len(input) > 8 then
        return { { 0, 8, "InputHighlight" } }
      else
        return {}
      end
    end,
  }, function(input)
    if input then
      print("You entered " .. input)
    else
      print "You cancelled"
    end
  end)
end
