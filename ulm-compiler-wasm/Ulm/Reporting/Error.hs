module Ulm.Reporting.Error
  ( Module(..)
  , Error(..)
--   , toDoc
--   , toJson
  )
  where


import qualified Data.ByteString as B
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified System.FilePath as FP

import qualified Elm.ModuleName as ModuleName
import qualified File
-- import qualified Json.Encode as E
-- import Json.Encode ((==>))
-- import qualified Reporting.Annotation as A
-- import qualified Reporting.Doc as D
-- import qualified Reporting.Error.Canonicalize as Canonicalize
-- import qualified Reporting.Error.Docs as Docs
-- import qualified Reporting.Error.Import as Import
-- import qualified Reporting.Error.Main as Main
-- import qualified Reporting.Error.Pattern as Pattern
-- import qualified Reporting.Error.Syntax as Syntax
-- import qualified Reporting.Error.Type as Type
-- import qualified Reporting.Render.Code as Code
-- import qualified Reporting.Render.Type.Localizer as L
-- import qualified Reporting.Report as Report



-- MODULE


data Module =
  Module
    { _name :: ModuleName.Raw
    , _absolutePath :: FilePath
    , _modificationTime :: File.Time
    , _source :: B.ByteString
    , _error :: Error
    }



-- ERRORS


data Error
--   = BadSyntax Syntax.Error
--   | BadImports (NE.List Import.Error)
--   | BadNames (OneOrMore.OneOrMore Canonicalize.Error)
--   | BadTypes L.Localizer (NE.List Type.Error)
--   | BadMains L.Localizer (OneOrMore.OneOrMore Main.Error)
--   | BadPatterns (NE.List Pattern.Error)
--   | BadDocs Docs.Error
    = Error {}
