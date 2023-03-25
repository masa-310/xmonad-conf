module Config (MyConfig, mkConfigByHost, getTerminal, getPromptConf, getProjects, getFont, getModMask) where

import Data.Default.Class
import XMonad (KeyMask, mod1Mask, mod4Mask, xK_Tab, xK_grave, spawn)
import XMonad.Actions.DynamicProjects (Project (..))
import XMonad.Prompt as Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

type HostName = String

defaultFont = "Inconsolata Nerd Font Mono"

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
    {
      projects =
        [ Project
            { projectName = "home",
              projectDirectory = "~",
              projectStartHook = Nothing
            }
        ,  Project
            { projectName = "ng-fe-elm",
              projectDirectory = "~/project/ng-fe-elm",
              projectStartHook = Just $ spawn "tmux"
            }
        ,  Project
            { projectName = "ng-be-node",
              projectDirectory = "~/project/ng-be-node",
              projectStartHook = Just $ spawn "tmux"
            }
        ,  Project
            { projectName = "obsidian",
              projectDirectory = "~",
              projectStartHook = Just $ spawn "obsidian"
            }
        ,  Project
            { projectName = "slack",
              projectDirectory = "~",
              projectStartHook = Just $ spawn "slack"
            }
        ]
    }

stinkyHuskyConfig :: Config
stinkyHuskyConfig =
  def
    {
      projects =
        [ Project
            { projectName = "home",
              projectDirectory = "~",
              projectStartHook = Nothing
            }
        ,  Project
            { projectName = "ng-be-scala",
              projectDirectory = "~/project/ng-backend-scala",
              projectStartHook = Just $ spawn "tmux"
            }
        ,  Project
            { projectName = "ng-be-node",
              projectDirectory = "~/project/ng-back-node",
              projectStartHook = Just $ spawn "tmux"
            }
        ,  Project
            { projectName = "ng-front-elm",
              projectDirectory = "~/project/ng-front-elm",
              projectStartHook = Just $ spawn "tmux"
            }
        ,  Project
            { projectName = "obsidian",
              projectDirectory = "~",
              projectStartHook = Just $ spawn "obsidian"
            }
        ,  Project
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
              height = 22,
              historySize = 256,
              Prompt.font = defaultFont,
              -- , bgColor = myBackgroundColor
              -- , fgColor = myContentColor
              -- , bgHLight = myBackgroundColor
              -- , fgHLight = myContentColor
              -- , borderColor = myBackgroundColor
              position = Bottom,
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
              defaultText = []
            }
      }

mkConfigByHost :: HostName -> Config
mkConfigByHost hostName =
  case hostName of
    "gooey-terrier" -> gooeyTerrierConfig
    "sulky-shiba" -> sulkyShibaConfig
    "stinky-husky" -> stinkyHuskyConfig
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
