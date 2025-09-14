-- |Entry point for the notebook plugin.
module Ide.Plugin.Notebook (
  descriptor,
  Log(..)
) where

import           Control.Monad.IO.Class

import           Development.IDE               (IdeState)
import           Ide.Logger                    (Recorder, WithPriority)
import           Ide.Types                     (ConfigDescriptor(..),
                                                PluginDescriptor(..), PluginId,
                                                defaultConfigDescriptor,
                                                defaultPluginDescriptor,
                                                mkCustomConfig, mkPluginNotificationHandler)
import qualified Language.LSP.Protocol.Message         as LSP
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types           as LSP
import           Ide.Logger



data Log 
  = LogOpenedNotebookDocument  !Uri
  | LogChangedNotebookDocument !Uri
  | LogClosedNotebookDocument  !Uri
  | LogSavedNotebookDocument   !Uri


descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "Provides notebook document support")
        { pluginNotificationHandlers = mconcat 
            [ mkPluginNotificationHandler LSP.SMethod_NotebookDocumentDidOpen (notebookDocumentDidOpen recorder)
            , mkPluginNotificationHandler LSP.SMethod_NotebookDocumentDidChange (notebookDocumentDidChange recorder)
            , mkPluginNotificationHandler LSP.SMethod_NotebookDocumentDidClose (notebookDocumentDidClose recorder)
            , mkPluginNotificationHandler LSP.SMethod_NotebookDocumentDidSave (notebookDocumentDidSave recorder)
            ]
        }

notebookDocumentDidOpen :: MonadIO m => Recorder (WithPriority Log) -> IdeState -> b -> c -> DidOpenNotebookDocumentParams -> m ()
notebookDocumentDidOpen recorder _ide _vfs _ (DidOpenNotebookDocumentParams NotebookDocument{ _uri, _version, _cells} _) 
  = liftIO $ do
      logWith recorder Debug $ LogOpenedNotebookDocument _uri

notebookDocumentDidChange :: MonadIO m => Recorder (WithPriority Log) -> IdeState -> b -> c -> DidChangeNotebookDocumentParams -> m ()
notebookDocumentDidChange recorder _ide _vfs _ (DidChangeNotebookDocumentParams identifier@VersionedNotebookDocumentIdentifier{_uri} changes) 
  = liftIO $ do
      logWith recorder Debug $ LogChangedNotebookDocument _uri

notebookDocumentDidClose :: MonadIO m => Recorder (WithPriority Log) -> IdeState -> b -> c -> DidCloseNotebookDocumentParams -> m ()
notebookDocumentDidClose recorder _ide _vfs _ (DidCloseNotebookDocumentParams NotebookDocumentIdentifier{_uri} _) 
  = liftIO $ do
      logWith recorder Debug $ LogClosedNotebookDocument _uri

notebookDocumentDidSave :: MonadIO m => Recorder (WithPriority Log) -> IdeState -> b -> c -> DidSaveNotebookDocumentParams -> m ()
notebookDocumentDidSave recorder _ide _vfs _ (DidSaveNotebookDocumentParams NotebookDocumentIdentifier{_uri}) 
  = liftIO $ do
      logWith recorder Debug $ LogSavedNotebookDocument _uri

