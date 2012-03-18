
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

-- | I, Chris Monsanto <chris@monsan.to>, wrote this code. It's under GPLv3.
-- 
-- TL;DR: BigDump.hs dumps a bunch of information about your Haskell files into Lisp lists.
--        Using this information, we provide goto-definition/autocomplete/etc in your Emacs.
-- 
-- A design decision is to *not* send ASTs to Emacs. Haskell is way better at list processing
-- than Emacs is :) Seriously, where's the pattern matching? If you want to customize how it works,
-- customize the Haskell part, not the Emacs part...
-- 

import qualified Language.Haskell.Exts.Annotated as L
import qualified Language.Haskell.Exts.Pretty    as Pretty
import qualified Language.Preprocessor.Cpphs     as CPP
import qualified Data.Map                        as Map

import           Prelude                         hiding ((.), id)
import           Control.Category
import           Data.Maybe
import           Data.List
import           Data.Label
import           Text.Printf
import           Data.Ord

import           System.Environment
import           System.IO
import           System.Directory
import           Control.Monad

-- Tags ------------------------------------------------------------------------

data InstanceData = InstanceDecl
                  | ClassMember String
                  | NotInstance
                    deriving (Show, Eq, Ord)

data Ident = Ident {
  _identStr :: String,             -- ^What is the name of the object in question?
  _identModule :: Maybe String,    -- ^What is the scoping?
  _identInstanceOf :: InstanceData -- ^Is this an instance? Of what?
  } deriving (Show, Eq, Ord)
                   
data Tag = Tag {
  _tagPos :: (Int, Int),          -- ^Where is this object? (line, column)
  _tagParent :: Maybe Ident,       -- ^This is only for export definitions.
  _tagSignature :: Maybe String,  -- ^Tell me about this object.
  _tagQuickHelp :: Maybe String   -- ^... in English, please...
  } deriving Show
             
newIdent s = Ident s Nothing NotInstance 
newTag loc = Tag loc Nothing Nothing Nothing

mergeTag :: Tag -> Tag -> Tag
mergeTag (Tag _ _ s _) (Tag p l s' h) = Tag p l (maybe s Just s') h
  
$(mkLabels [''Ident, ''Tag])

-- Database --------------------------------------------------------------------

-- | Here is our database of identifiers.
type Database = Map.Map Ident Tag
data Export = ExpString String | ExpAll String | ExpSome String [String] | ExpModule String
data Import = Import {
  impModule :: String,
  impList :: [Export],
  impAlias :: Maybe String,
  impQualified :: Bool,
  impHiding :: Bool
  }
type Result = (String, [Import], [Export], Database)

-- | Combine two databases.
merge :: Database -> Database -> Database
merge = Map.unionWith mergeTag

mergeWithParent :: Database -> Database -> Database
mergeWithParent m m' = Map.unionWith mergeTag m (Map.map (set tagParent (Just $ head $ Map.keys m)) m') 

merges :: (a -> Database) -> [a] -> Database
merges f = Map.unionsWith mergeTag . map f

haskellSource :: FilePath -> IO String
haskellSource file = do
    contents <- readFile file
    CPP.runCpphs cppOpts file contents
    where
    cppOpts = CPP.defaultCpphsOptions { CPP.boolopts = CPP.defaultBoolOptions { CPP.hashline = False } }

createTags :: FilePath -> String -> Either Result String
createTags file s = 
  case L.parseFileContentsWithComments mode s of
    L.ParseOk (m, comments) -> 
      case extractModule m of
        ((line, _), (mod, imps, exps, db)) -> 
          Left (mod, imps, exps, commentsFixUp db $
                                 dropWhile (\((line', _), _) -> line > line') $ 
                                 map extractComment comments) 
    L.ParseFailed _ s -> Right s
  where
    mode = L.ParseMode {
      L.parseFilename = file,
      L.extensions = L.knownExtensions,
      L.ignoreLanguagePragmas = False,
      L.ignoreLinePragmas = False,
      L.fixities = Nothing
      }
  
  -- | Set the signature of the database.         
setSig :: (Pretty.Pretty a) => a -> Database -> Database
setSig k = Map.map (set tagSignature . Just $ pretty k)

setModule :: String -> Database -> Database
setModule s = Map.mapKeys (set identModule $ Just s)


-- | Add a context to the signature.
mapSigCtxt :: (Pretty.Pretty a) => a -> Database -> Database
mapSigCtxt k = Map.map (modify tagSignature (fmap (printf ctxtPretty (pretty k) ++)))

-- Comments

extractComment :: L.Comment -> ((Int, Int), String)
extractComment (L.Comment _ span s) = (L.srcSpanStart span, s)

commentsFixUp :: Database -> [((Int, Int), String)] -> Database
commentsFixUp db comments = Map.fromList $ commentsFixUp' objs comments
  where
    objs = sortBy (comparing (get tagPos . snd)) $ Map.toList db
    commentsFixUp' objs [] = objs
    commentsFixUp' [] _ = []
    commentsFixUp' ((ident, tag) : objs) comments =
      let 
        (line, column) = get tagPos tag 
        (aboveComments, rest) = span (\((line', column'), str) -> line > line') comments 
        sideComments = filter (\((line', column'), str) -> line == line' && column' > column) rest
      in
       (ident, set tagQuickHelp (Just $ extractHaddock aboveComments sideComments) tag) : commentsFixUp' objs rest

killIndent :: String -> String
killIndent s = let l = reverse $ dropWhile (== "") $ reverse $ dropWhile (== "") $ lines s
                   minLength = foldl min 9999 (map (length . takeWhile (== ' ')) $ filter (/= "") l)
                   in
                unlines $ map (drop minLength) l

                
extractHaddock :: [((Int, Int), String)] -> [((Int, Int), String)] -> String
extractHaddock acoms scoms = 
  case (acom, scom) of 
    (_, Just s) -> s
    (Just s, _) -> s
    (_, _) -> ""
  where
    acom = case dropWhile (not . (isPrefixOf " |")) $ map snd acoms of 
      [] -> Nothing 
      (x : xs) -> Just (killIndent $ unlines ((fromJust $ stripPrefix " |" x) : xs))
    scom = case scoms of 
      [(_, s)] -> case stripPrefix " ^" s of 
          Just x -> Just $ dropWhile (== ' ') x
          Nothing -> Nothing
      _ -> Nothing

-- Prettying -------------------------------------------------------------------

-- | What is the prefix for a context?
ctxtPretty = "[%s]  "

-- | Display an object very prettily. 
pretty = Pretty.prettyPrintStyleMode 
         Pretty.style 
         Pretty.defaultMode { Pretty.classIndent = 4, 
                              Pretty.spacing = False}

-- Extraction ------------------------------------------------------------------

type Span = L.SrcSpanInfo

-- | Re-exports, exported database, all database.
extractModule :: L.Module Span -> ((Int, Int), Result)
extractModule (L.Module _ mh _ importdecls decls) = 
  case mh of
    Just k@(L.ModuleHead loc (L.ModuleName _ s) _ exportspec) -> 
      (extractLoc loc, (s, 
                        map extractImport importdecls, 
                        map extractExportSpec (maybe [] (\(L.ExportSpecList _ l) -> l) exportspec), 
                        setModule s decls'))
    Nothing -> ((0, 0), ("", [], [], decls'))
  where
    decls' = merges extractDecl decls
extractModule _ = error "no"

-- TODO don't forget to make a concat
fromName :: L.Name Span -> String
fromName (L.Ident _ s) = s
fromName (L.Symbol _ s) = s

fromQName :: L.QName Span -> String
fromQName (L.Qual _ (L.ModuleName _ s) n) = s ++ "." ++ fromName n
fromQName (L.UnQual _ n) = fromName n
fromQName (L.Special {}) = ""

fromCName :: L.CName Span -> String
fromCName (L.VarName _ n) = fromName n
fromCName (L.ConName _ n) = fromName n

extractExportSpec :: L.ExportSpec Span -> Export
extractExportSpec (L.EVar _ q) = ExpString (fromQName q) 
extractExportSpec (L.EAbs _ q) = ExpString (fromQName q) 
extractExportSpec (L.EThingAll _ q) = ExpAll (fromQName q) 
extractExportSpec (L.EThingWith _ q cs) = ExpSome (fromQName q) (map fromCName cs)
extractExportSpec (L.EModuleContents _ (L.ModuleName _ s)) = ExpModule s

extractImportSpec :: L.ImportSpec Span -> Export
extractImportSpec (L.IVar _ n) = ExpString (fromName n) 
extractImportSpec (L.IAbs _ n) = ExpString (fromName n) 
extractImportSpec (L.IThingAll _ n) = ExpAll (fromName n) 
extractImportSpec (L.IThingWith _ n cs) = ExpSome (fromName n) (map fromCName cs)

-- importspeclist _ hide importspecs
-- importspec 

extractImport :: L.ImportDecl Span -> Import
extractImport (L.ImportDecl _ (L.ModuleName _ s) qual _ _ alias importlist) =
  case importlist of 
    Just (L.ImportSpecList _ hidden impspecs) -> Import s (map extractImportSpec impspecs) alias' qual hidden  
    Nothing -> Import s [] alias' qual False
  where
    alias' = fmap (\(L.ModuleName _ s) -> s) alias

extractDecl :: L.Decl Span -> Database
extractDecl k@(L.TypeDecl _ head type_) = 
  setSig k (extractDeclHead head)
extractDecl k@(L.TypeFamDecl _ head kind) = 
  setSig k (extractDeclHead head) 
extractDecl k@(L.DataDecl _ _ ctxt head decls _) = 
  setSig k (extractDeclHead head) `mergeWithParent` mapSigCtxt head (merges extractQualConDecl decls)
extractDecl k@(L.GDataDecl _ _ ctxt head kind decls _) = 
  setSig k (extractDeclHead head) `mergeWithParent` mapSigCtxt head (merges extractGadtDecl decls)
extractDecl k@(L.DataFamDecl _ ctxt head kind) = 
  setSig k (extractDeclHead head) 
extractDecl k@(L.ClassDecl src ctxt head deps clsdecls) = 
   setSig (L.ClassDecl src ctxt head deps (Just $ decls)) (extractDeclHead head) 
           `mergeWithParent` 
   mapSigCtxt head (merges extractClassDecl decls)
   where
     decls = mapMaybe doctorClassDecl $ fromMaybe [] clsdecls
extractDecl k@(L.TypeSig _ names type_) = 
  setSig k $ merges extractName names
extractDecl k@(L.FunBind _ (L.Match _ name _ _ _ : _)) = 
  extractName name
extractDecl k@(L.FunBind _ (L.InfixMatch _ _ name _ _ _ : _)) = 
  extractName name
extractDecl (L.PatBind _ pat _ _ _) = 
  extractPat pat
extractDecl (L.InstDecl loc ctxt head instdecls) =
  Map.mapKeys (set identInstanceOf $ InstanceDecl) 
  (setSig (L.InstDecl loc ctxt head Nothing) (Map.singleton (newIdent (pretty head)) (newTag (extractLoc loc))))
  `merge`
  Map.mapKeys (set identInstanceOf $ ClassMember (pretty head)) (merges extractInstDecl (fromMaybe [] instdecls))
extractDecl k@(L.ForImp _ _ _ _ name type_) = 
  setSig k (extractName name)
extractDecl _ = Map.empty

extractInstDecl :: L.InstDecl Span -> Database
extractInstDecl (L.InsDecl _ decl) = extractDecl decl
extractInstDecl _ = Map.empty

extractDeclHead :: L.DeclHead Span -> Database
extractDeclHead k@(L.DHead _ name vars) = 
  setSig k $ extractName name
extractDeclHead k@(L.DHInfix _ var1 name var2) = 
  setSig k $ extractName name
extractDeclHead (L.DHParen _ head') = 
  extractDeclHead head'

extractQualConDecl :: L.QualConDecl Span -> Database
extractQualConDecl k@(L.QualConDecl _ _ ctxt (L.ConDecl _ name btypes)) = 
  setSig k (extractName name)
extractQualConDecl k@(L.QualConDecl _ _ ctxt (L.InfixConDecl _ btype1 name btype2)) = 
  setSig k (extractName name)
extractQualConDecl k@(L.QualConDecl _ _ ctxt (L.RecDecl _ name fields)) = 
  setSig k (extractName name) `merge` merges extractFieldDecl fields
extractFieldDecl :: L.FieldDecl Span -> Database
extractFieldDecl k@(L.FieldDecl _ names btype) = 
  setSig k (merges extractName names)

extractGadtDecl :: L.GadtDecl Span -> Database
extractGadtDecl k@(L.GadtDecl _ name type_) = 
  setSig k (extractName name) 
  
extractClassDecl :: L.ClassDecl Span -> Database
extractClassDecl k@(L.ClsDecl _ decl) = extractDecl decl
extractClassDecl k@(L.ClsDataFam _ ctxt head kind) = 
  setSig k (extractDeclHead head)
extractClassDecl k@(L.ClsTyFam _ head kind) = 
  setSig k (extractDeclHead head)
extractClassDecl _ = Map.empty

extractPat :: L.Pat Span -> Database
extractPat (L.PVar _ name) = extractName name
extractPat (L.PApp _ _ pats) = merges extractPat pats
extractPat (L.PTuple _ pats) = merges extractPat pats
extractPat (L.PList _ pats) = merges extractPat pats
extractPat (L.PParen _ pat) = extractPat pat
extractPat (L.PAsPat _ name pat) = extractName name `merge` extractPat pat
extractPat (L.PIrrPat _ pat) = extractPat pat
extractPat (L.PatTypeSig _ pat _) = extractPat pat
extractPat (L.PBangPat _ pat) = extractPat pat
extractPat _ = Map.empty

extractName :: L.Name Span -> Database
extractName (L.Ident loc name) = Map.singleton (newIdent name) (newTag (extractLoc loc))
extractName (L.Symbol loc name) = Map.singleton (newIdent name) (newTag (extractLoc loc))

extractLoc :: Span -> (Int, Int)
extractLoc (L.SrcSpanInfo (L.SrcSpan _ line column _ _) _) = (line, column)

-- Doctoring -------------------------------------------------------------------

doctorClassDecl :: L.ClassDecl Span -> Maybe (L.ClassDecl Span)
doctorClassDecl k@(L.ClsDecl src (L.TypeSig src' names types)) = Just k
doctorClassDecl k@(L.ClsDataFam {}) = Just k
doctorClassDecl k@(L.ClsTyFam {}) = Just k
doctorClassDecl _ = Nothing               

-- Export ----------------------------------------------------------------------

quote :: String -> String
quote s = "\"" ++ quote' s ++ "\""
  where
    quote' [] = []
    quote' ('"' : xs) = "\\\"" ++ quote' xs
    quote' ('\\' : xs) = "\\\\" ++ quote' xs 
    quote' (x : xs) = x : quote' xs

tplList :: [String] -> String
tplList l = "(list " ++ intercalate "\n" l ++ ")"

tplMaybe :: Maybe String -> String
tplMaybe = fromMaybe "nil"

tplBool :: Bool -> String
tplBool True = "t"
tplBool False = "nil"

tplExport :: Export -> String
tplExport (ExpString s) = printf "(list 'id %s)" (quote s)
tplExport (ExpAll s) = printf "(list 'all %s)" (quote s)
tplExport (ExpSome s xs) = printf "(list 'some %s %s)" (quote s) (tplList xs)
tplExport (ExpModule s) = printf "(list 'mod %s)" (quote s)

tplImport :: Import -> String 
tplImport i = printf
 (unlines ["(make-hieimport",
          ":name %s",
          ":list %s",
          ":alias %s",
          ":is-qualified %s",
          ":is-hide %s)"])
 (quote $ impModule i) 
 (tplList (map tplExport $ impList i)) 
 (tplMaybe (fmap quote $ impAlias i)) 
 (tplBool $ impQualified i)  
 (tplBool $ impHiding i)
 
tplMakeName :: String -> InstanceData -> String 
tplMakeName s (ClassMember c) = s ++ " / " ++ c
tplMakeName s _ = s

tplInstanceOf :: InstanceData -> String
tplInstanceOf NotInstance = "nil"
tplInstanceOf _ = "t"

tplLocalDef :: FilePath -> (Ident, Tag) -> String
tplLocalDef file (Ident name mod inst, Tag (line, _) parent sig qh) = printf
 (unlines ["(make-hiedef",
          ":name %s",
          ":module %s",
          ":is-instance %s",
          ":parent %s",
          ":file %s",
          ":line %i",
          ":signature %s",
          ":help %s)"])
          (quote (tplMakeName name inst)) 
          (tplMaybe (fmap quote mod))
          (tplMaybe (fmap (quote . get identStr) parent)) 
          (tplInstanceOf inst)
          (quote file)
          line 
          (tplMaybe (fmap quote sig)) 
          (tplMaybe (fmap quote qh)) 
           
elisp :: FilePath -> Result -> String
elisp file (mod, imps, exps, db) = printf 
                                   (unlines ["(setq *hie-module-name* %s)",
                                             "(setq *hie-locally-defined* %s)",
                                             "(setq *hie-imports* %s)",
                                             "(setq *hie-exports* %s)"])
                                   (quote mod)
                                   (tplList $ map (tplLocalDef file) $ Map.toList db)
                                   (tplList $ map tplImport imps)
                                   (tplList $ map tplExport exps)

-- Driver ----------------------------------------------------------------------

usage = putStrLn "Usage: hie file.hs dump.el"

main :: IO ()
main = do 
  files <- getArgs
  case files of 
    [filein, fileout] -> do
      contents <- haskellSource filein
      case createTags filein contents of 
        Left res -> 
          do path <- canonicalizePath filein
             writeFile fileout (elisp path res)
        Right s -> putStrLn s
    _ -> usage
  
                