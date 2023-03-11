return {
  -- {
  --   -- WORD USAGE HIGHLIGHTER
  --   "RRethy/vim-illuminate",
  --   config = function()
  --     require('illuminate').configure({
  --       delay = 200,
  --       under_cursor = false,
  --       min_count_to_highlight = 2,
  --     })
  --   end
  -- },
  {
    -- JUMP TO WORD INDICTORS
    "jinh0/eyeliner.nvim",
    config = function()
      require 'eyeliner'.setup {
        highlight_on_key = true
      }
    end
  },
  {
    -- CURSOR MOVEMENT HIGHLIGHTER
    "DanilaMihailov/beacon.nvim"
  },
  {
    -- HIGHLIGHT YANKED REGION
    "machakann/vim-highlightedyank"
  }
}
