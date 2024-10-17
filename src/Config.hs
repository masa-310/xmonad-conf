module Config (MyConfig, mkConfigByHost, getTerminal, getPromptConf, getProjects, getFont, getModMask) where

import Data.Default.Class
import XMonad (KeyMask, mod1Mask, mod4Mask, spawn, xK_Tab, xK_grave)
import XMonad.Actions.DynamicProjects (Project (..))
import XMonad.Prompt as Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

type HostName = String

defaultFont = "xft:Inconsolata Nerd Font Mono"

data Config = Config
  { terminal :: String,
    font :: String,
    projects :: [Project],
    promptConfig :: XPConfig,
    modMask :: KeyMask
  }

type MyConfig = Config

gooeyTerrierConfig :: Config
gooeyTerrierConfig = def

sulkyShibaConfig :: Config
sulkyShibaConfig =
  def
    { projects =
        [ Project
            { projectName = "home",
              projectDirectory = "~",
              projectStartHook = Nothing
            },
          Project
            { projectName = "ng-fe-elm",
              projectDirectory = "~/project/ng-fe-elm",
              projectStartHook = Just $ spawn "tmux"
            },
          Project
            { projectName = "ng-be-node",
              projectDirectory = "~/project/ng-be-node",
              projectStartHook = Just $ spawn "tmux"
            },
          Project
            { projectName = "obsidian",
              projectDirectory = "~",
              projectStartHook = Just $ spawn "obsidian"
            },
          Project
            { projectName = "slack",
              projectDirectory = "~",
              projectStartHook = Just $ spawn "slack"
            }
        ]
    }

stinkyHuskyConfig :: Config
stinkyHuskyConfig =
  def
    { terminal = "wezterm"
    , projects =
        [ Project
            { projectName = "home",
              projectDirectory = "~",
              projectStartHook = Nothing
            },
          Project
            { projectName = "slack",
              projectDirectory = "~",
              projectStartHook = Just $ spawn "slack"
            }
        ]
    }

fishyRetrieverConfig :: Config
fishyRetrieverConfig =
  def
    { terminal = "wezterm"
    , modMask = mod1Mask
    , projects =
        [ Project
            { projectName = "home",
              projectDirectory = "~",
              projectStartHook = Nothing
            },
          Project
            { projectName = "slack",
              projectDirectory = "~",
              projectStartHook = Just $ spawn "slack"
            }
        ]
    }

flattenCollieConfig :: Config
flattenCollieConfig =
  def
    { terminal = "wezterm"
    , modMask = mod1Mask
    , projects =
        [ Project
            { projectName = "home",
              projectDirectory = "~",
              projectStartHook = Nothing
            },
          Project
            { projectName = "slack",
              projectDirectory = "~",
              projectStartHook = Just $ spawn "slack"
            }
        ]
    }

instance Default Config where
  def =
    Config
      { terminal = "alacritty",
        Config.font = defaultFont,
        modMask = mod4Mask,
        projects =
          [ Project
              { projectName = "home",
                projectDirectory = "~",
                projectStartHook = Nothing
              }
          ],
        promptConfig =
          def
            { promptBorderWidth = 1,
              alwaysHighlight = True,
              historySize = 256,
              Prompt.font = defaultFont,
              -- , bgColor = myBackgroundColor
              -- , fgColor = myContentColor
              -- , bgHLight = myBackgroundColor
              -- , fgHLight = myContentColor
              -- , borderColor = myBackgroundColor
              position = CenteredAt 0.5 0.5,
              autoComplete = Just 100,
              showCompletionOnTab = False,
              searchPredicate = fuzzyMatch,
              defaultPrompter = id,
              sorter = const id,
              maxComplRows = Just 7,
              promptKeymap = defaultXPKeymap,
              completionKey = (0, xK_Tab),
              changeModeKey = xK_grave,
              historyFilter = id,
              height = 100,
              defaultText = []
            }
      }

mkConfigByHost :: HostName -> Config
mkConfigByHost hostName =
  case hostName of
    "stinky-husky" -> stinkyHuskyConfig
    "fishy-retriever" -> fishyRetrieverConfig
    "flatten-collie" -> flattenCollieConfig
    "default" -> def
    _ -> def

getTerminal :: Config -> String
getTerminal = terminal

getPromptConf :: Config -> XPConfig
getPromptConf = promptConfig

getFont :: Config -> String
getFont = Config.font

getProjects :: Config -> [Project]
getProjects = projects

getModMask :: Config -> KeyMask
getModMask = modMask
