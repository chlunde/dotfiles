--- print("hello, world");
vim.g.mapleader = " "

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex, { desc = "Browse files" })

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

--- vim.keymap.set("n", "<leader>p", [["_dP]])

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    {
        'folke/which-key.nvim',
        event = 'VeryLazy',
        init = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 300
        end,
        opts = {
            -- your configuration comes here
            -- or leave it empty to use the default settings
            -- refer to the configuration section below
        }
    },
    {
        'VonHeikemen/lsp-zero.nvim',
        commit = 'e04f88174bf1ea439e40b02d9dd5158b3c0bb7ec',
        lazy = true,
        config = false,
    },
    {
        'neovim/nvim-lspconfig',
        commit = 'bfdf2e91e7297a54bcc09d3e092a12bff69a1cf4',
        dependencies = {
            {
                'hrsh7th/cmp-nvim-lsp',
                commit = '44b16d11215dce86f253ce0c30949813c0a90765',
            },
        }
    },
    -- Autocompletion
    {
        'hrsh7th/nvim-cmp',
        commit = '5dce1b778b85c717f6614e3f4da45e9f19f54435',
        dependencies = {
            {
                'L3MON4D3/LuaSnip',
                commit = '480b032f6708573334f4437d3f83307d143f1a72',
            }
        },
    },
    { 
        'rose-pine/neovim',
        name = 'rose-pine',
        commit = 'e29002cbee4854a9c8c4b148d8a52fae3176070f',
    },
    {
        'nvim-telescope/telescope.nvim',
        commit = '54930e1abfc94409e1bb9266e752ef8379008592',
        dependencies = {
            {
                'nvim-lua/plenary.nvim',
                commit = '9ce85b0f7dcfe5358c0be937ad23e456907d410b',
            }
        },
    },
    {
        'nvim-treesitter/nvim-treesitter',
        commit = '69388e84c34d40c3d5c7d2f310db13276f2179e1',
	-- TSUpdate
    },
    {
        'nvim-treesitter/nvim-treesitter-context',
        commit = 'ce583c89c8db8d34cd5dff0dc91e13b446fdbe50',
    },
    {
        'mbbill/undotree',
        commit = '0e11ba7325efbbb3f3bebe06213afa3e7ec75131',
    },
    {
        'theprimeagen/harpoon',
        commit = '21f4c47c6803d64ddb934a5b314dcb1b8e7365dc',
    },
    {
        'tpope/vim-fugitive',
    },
    {
        'github/copilot.vim',
    },
    {
        'crispgm/nvim-go',
    },
}
)

vim.cmd('colorscheme rose-pine')

require('go').setup({
    linter = 'staticcheck',
    formatter = 'gofumpt',
})

local lsp_zero = require('lsp-zero')

lsp_zero.on_attach(function(client, bufnr)
    -- see :help lsp-zero-keybindings
    -- to learn the available actions
    lsp_zero.default_keymaps({buffer = bufnr})
end)

require('lspconfig').gopls.setup({
    settings = {
        gopls = {
            completeUnimported = true,
            usePlaceholders = true,
            analyses = {
                unusedparams = true,
            },
            staticcheck = true,
            vulncheck = "Imports",
            gofumpt = true,
        },
    },
})
require('lspconfig').tsserver.setup({})
require('lspconfig').rust_analyzer.setup({})

require("set")

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>pf', builtin.find_files, { desc = "Find Files" })
vim.keymap.set('n', '<leader>pg', builtin.live_grep, { desc = "Live Grep"})
vim.keymap.set('n', '<leader>pb', builtin.buffers, { desc = "Find Buffers"})
vim.keymap.set('n', '<leader>pB', builtin.git_branches, { desc = "Branches"})
vim.keymap.set('n', '<C-p>', builtin.git_files, {})
vim.keymap.set('n', '<leader>ps', function()
	builtin.grep_string({ search = vim.fn.input("Grep > ") })
end, { desc = "Grep files" })
vim.keymap.set('n', '<leader>vh', builtin.help_tags, { desc = "Browse Help" })

require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all"
  ensure_installed = { "go", "javascript", "typescript", "c", "lua", "rust" },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
  auto_install = true,

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}

vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle, { desc = "Undo tree" })

local lsp = require("lsp-zero")

local cmp = require('cmp')
local cmp_select = {behavior = cmp.SelectBehavior.Select}
local cmp_mappings = lsp.defaults.cmp_mappings({
  ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
  ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
  ['<C-y>'] = cmp.mapping.confirm({ select = true }),
  ["<C-x>"] = cmp.mapping.complete(),
})

cmp_mappings['<Tab>'] = nil
cmp_mappings['<S-Tab>'] = nil

lsp.setup({
  mapping = cmp.mapping.preset.insert(cmp_mappings),
})

lsp.set_preferences({
    suggest_lsp_servers = false,
    sign_icons = {
        error = 'E',
        warn = 'W',
        hint = 'H',
        info = 'I'
    }
})

local augroup = vim.api.nvim_create_augroup('LspFormatting', {})
local lsp_format_on_save = function(bufnr)
  vim.api.nvim_clear_autocmds({group = augroup, buffer = bufnr})
  vim.api.nvim_create_autocmd('BufWritePre', {
      group = augroup,
      buffer = bufnr,
      callback = function()
          vim.lsp.buf.format()
      end,
  })
end

lsp.on_attach(function(client, bufnr)
    local opts = {buffer = bufnr, remap = false}

    lsp_format_on_save(bufnr)

    function opts_with_desc(desc)
        opts.desc = desc
        return opts
    end

  vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts_with_desc("Go to definition"))
  vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
  vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts_with_desc("Workspace symbol"))
  vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts_with_desc("Diagnostics"))

  vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts_with_desc("Next diagnostics"))
  vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts_with_desc("Prev diagnostics"))
  vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts_with_desc("Code Action"))
  vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts_with_desc("References"))
  vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts_with_desc("Rename"))
  vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts_with_desc("Signature help"))
end)

lsp.setup()

vim.diagnostic.config({
    virtual_text = true
})

local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

vim.keymap.set("n", "<leader>a", mark.add_file, {desc = "Add file to harpoon"})
vim.keymap.set("n", "<C-e>", ui.toggle_quick_menu, {desc = "Toggle harpoon menu"})

vim.keymap.set("n", "<leader>1", function() ui.nav_file(1) end, {desc="Harpoon 1"})
vim.keymap.set("n", "<leader>2", function() ui.nav_file(2) end, {desc="Harpoon 2"})
vim.keymap.set("n", "<leader>3", function() ui.nav_file(3) end, {desc="Harpoon 3"})
vim.keymap.set("n", "<leader>4", function() ui.nav_file(4) end, {desc="Harpoon 4"})
vim.keymap.set("n", "<leader>]", function() ui.nav_next() end, {desc="Harpoon next"})
vim.keymap.set("n", "<leader>[", function() ui.nav_prev() end, {desc="Harpoon prev"})
