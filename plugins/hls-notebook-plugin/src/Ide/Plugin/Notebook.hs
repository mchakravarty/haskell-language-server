-- |Entry point for the notebook plugin.
module Ide.Plugin.Notebook (
  descriptor,
  Log(..)
) where

import           Development.IDE               (IdeState)
import           Ide.Logger                    (Recorder, WithPriority)
import           Ide.Types                     (ConfigDescriptor (..),
                                                PluginDescriptor (..), PluginId,
                                                defaultConfigDescriptor,
                                                defaultPluginDescriptor,
                                                mkCustomConfig, {-mkPluginHandler-})
--import           Language.LSP.Protocol.Message


data Log

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "Provides notebook document support")
        { pluginHandlers = mconcat
            [ -- mkPluginHandler SMethod_TextDocumentCodeAction (Handlers.codeAction recorder)
--            , mkPluginHandler SMethod_TextDocumentCodeLens (Handlers.codeLens recorder)
            ]
        }
