
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

-- | I, Chris Monsanto <chris@monsan.to>, wrote this code. It's under GPLv3.

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
import System.Exit
import Text.Regex




-- Tags ------------------------------------------------------------------------

data InstanceData = InstanceDecl
                  | ClassMember String
                  | NotInstance
                    deriving (Show, Eq, Ord)

data Ident = Ident {
  _identStr :: String,             -- ^What is the name of the object in question? 
  _identInstanceOf :: InstanceData -- ^Is this an instance? Of what?
  } deriving (Show, Eq, Ord)
                   
data Tag = Tag {
  _tagPos :: (Int, Int),          -- ^Where is this object? (line, column)
  _tagParent :: Maybe Ident,       -- ^This is only for export definitions.
  _tagSignature :: Maybe String,  -- ^Tell me about this object.
  _tagQuickHelp :: Maybe String   -- ^... in English, please...
  } deriving Show
             
newIdent s = Ident s NotInstance 
newTag loc = Tag loc Nothing Nothing Nothing

mergeTag :: Tag -> Tag -> Tag
mergeTag (Tag _ _ s _) (Tag p l s' h) = Tag p l (maybe s Just s') h
  

-- Database --------------------------------------------------------------------

-- | Here is our database of identifiers.
type Database = Map.Map Ident Tag
data Export = ExpString String | ExpAll String | ExpModule String
data Import = Import {
  _impModule :: String,
  _impList :: [Export],
  _impAlias :: String,
  _impQualified :: Bool,
  _impHiding :: Bool
  }
$(mkLabels [''Ident, ''Tag, ''Import])              
type Result = (String, [Import], [Export], Database)


-- | Combine two databases.
merge :: Database -> Database -> Database
merge = Map.unionWith mergeTag

mergeWithParent :: Database -> Database -> Database
mergeWithParent m m' = Map.unionWith mergeTag m (Map.map (set tagParent (Just $ head $ Map.keys m)) m') 


merges :: (a -> Database) -> [a] -> Database
merges f = Map.unionsWith mergeTag . map f


createTags :: FilePath -> String -> Either Result String
createTags file s = 
  case L.parseFileContentsWithComments mode s of
    L.ParseOk (m, comments) -> 
      case extractModule m of
        ((line, _), (mod, imps, exps, db)) -> 
          let db' = commentsFixUp db $
                    dropWhile (\((line', _), _) -> line > line') $ 
                    map extractComment comments
          in
           Left (mod, imps, exps, aliasModules mod db') 
    L.ParseFailed (L.SrcLoc _ l c) s -> Right (printf "%i, %i: %s" l c s)
  where
    exts = fromMaybe [] (L.readExtensions s)
    mode = L.ParseMode {
      L.parseFilename = "", -- we unlit before, so don't do it again.
      L.extensions = [
        L.PackageImports,
        L.TypeFamilies,  -- uses family keyword
        L.GADTs,
        L.StandaloneDeriving,
        L.TemplateHaskell,
        L.FunctionalDependencies,
        L.EmptyDataDecls,
        L.ExistentialQuantification,
        L.KindSignatures,
        L.BangPatterns,    -- uses ! as a keyword
        L.ScopedTypeVariables, 
        L.ViewPatterns, 
        L.NamedFieldPuns,
        L.RecordWildCards,
        L.ImplicitParams,         -- uses ? as a keyword, %keyword
        L.UnboxedTuples,          -- uses (#, #) as a keyword 
        L.MagicHash,     -- uses # as a keyword 
        L.QuasiQuotes,
        L.ExplicitForall,
        --L.Arrows,    -- -< and -<<
        L.UnicodeSyntax, 
        L.RecursiveDo,    -- mdo
        L.ForeignFunctionInterface,  -- foreign,
        L.PatternGuards,
        L.MultiParamTypeClasses,
        L.TypeOperators,
        L.TupleSections,
        L.FlexibleContexts] ++ exts,
      L.ignoreLanguagePragmas = False,
      L.ignoreLinePragmas = False,
      L.fixities = Nothing 
      }
  
  -- | Set the signature of the database.         
setSig :: (Pretty.Pretty a) => a -> Database -> Database
setSig k = Map.map (set tagSignature . Just $ pretty k)

aliasModules :: String -> Database -> Database 
aliasModules "" db = db
aliasModules s db = Map.union db (Map.mapKeys f db) 
  where
    f (Ident s' NotInstance) = (Ident (s ++ "." ++ s') NotInstance)
    f i = i

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

noPrelude = any (== "NoImplicitPrelude") . concat . mapMaybe (\x -> case x of 
                                                        L.LanguagePragma _ ns -> Just $ map fromName ns
                                                        _ -> Nothing)
  
addPrelude :: [Import] -> [Import]
addPrelude l | not $ any ((== "Prelude") . get impModule) l = Import "Prelude" [] "Prelude" False False : l
             | otherwise = l

-- | Re-exports, exported database, all database.
extractModule :: L.Module Span -> ((Int, Int), Result)
extractModule (L.Module _ mh prags importdecls decls) = 
  case mh of
    -- note we grab the end line and column, not the normal beginning and end.
    Just k@(L.ModuleHead (L.SrcSpanInfo (L.SrcSpan _ _ _ line column) _) (L.ModuleName _ s) _ exportspec) -> 
      ((line, column), 
       (s, imports', 
        case concatMap extractExportSpec (maybe [] (\(L.ExportSpecList _ l) -> l) exportspec) of
          [] -> [ExpModule s]
          xs -> xs, 
        decls'))
    Nothing -> ((0, 0), ("", imports', [], decls'))
  where
    imports = mapMaybe extractImport importdecls
    imports' | noPrelude prags = imports
             | otherwise = addPrelude imports
    decls' = merges extractDecl decls
extractModule _ = error "no"



-- TODO don't forget to make a concat
fromName :: L.Name Span -> String
fromName (L.Ident _ s) = s
fromName (L.Symbol _ s) = s

fromQName :: L.QName Span -> (String, String)
fromQName (L.Qual _ (L.ModuleName _ s) n) = (s, fromName n)
fromQName (L.UnQual _ n) = ("", fromName n)
fromQName (L.Special {}) = ("", "")

qualit ("", y) = y
qualit (x, y) = x ++ "." ++ y

fromCName :: L.CName Span -> String
fromCName (L.VarName _ n) = fromName n
fromCName (L.ConName _ n) = fromName n

extractExportSpec :: L.ExportSpec Span -> [Export]
extractExportSpec (L.EVar _ q) = [ExpString (qualit $ fromQName q)]
extractExportSpec (L.EAbs _ q) = [ExpString (qualit $ fromQName q)]
extractExportSpec (L.EThingAll _ q) = [ExpAll (qualit $ fromQName q)]
extractExportSpec (L.EThingWith _ q cs) = ExpString (qualit (x, y)) : 
                                          map (\y' -> ExpString $ qualit (x, fromCName y')) cs 
  where 
    (x, y) = fromQName q
extractExportSpec (L.EModuleContents _ (L.ModuleName _ s)) = [ExpModule s]

extractImportSpec :: L.ImportSpec Span -> [Export]
extractImportSpec (L.IVar _ n) = [ExpString (fromName n)]
extractImportSpec (L.IAbs _ n) = [ExpString (fromName n)] 
extractImportSpec (L.IThingAll _ n) = [ExpAll (fromName n)]
extractImportSpec (L.IThingWith _ n cs) = ExpString (fromName n) : 
                                          map (ExpString . fromCName) cs

extractImport :: L.ImportDecl Span -> Maybe Import
extractImport (L.ImportDecl _ (L.ModuleName _ s) qual issrc _ alias importlist) 
  | not issrc = Just $ case importlist of 
    Just (L.ImportSpecList _ hidden impspecs) -> 
      Import s (concatMap extractImportSpec impspecs) alias' qual hidden  
    Nothing -> Import s [] alias' qual True
  | otherwise = Nothing
  where
    alias' = maybe s (\(L.ModuleName _ s) -> s) alias

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
extractPat (L.PTuple _ _ pats) = merges extractPat pats
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
tplExport (ExpModule s) = printf "(list 'mod %s)" (quote s)

tplImport :: Import -> String 
tplImport i = printf
 (unlines ["(make-hieimport",
          ":name %s",
          ":list %s",
          ":alias %s", 
          ":is-qualified %s",
          ":is-hidden %s)"])
 (quote $ get impModule i) 
 (tplList (map tplExport $ get impList i)) 
 (quote $ get impAlias i)
 (tplBool $ get impQualified i)  
 (tplBool $ get impHiding i)
 
tplMakeName :: String -> InstanceData -> String 
tplMakeName s (ClassMember c) = s ++ " / " ++ c
tplMakeName s _ = s

tplInstanceOf :: InstanceData -> String
tplInstanceOf NotInstance = "nil"
tplInstanceOf _ = "t"

tplLocalDef :: FilePath -> (Ident, Tag) -> String
tplLocalDef file (Ident name inst, Tag (line, column) parent sig qh) = printf
 (unlines ["(make-hiedef",
          ":name %s",
          ":is-instance %s",
          ":parent %s",
          ":file %s",
          ":line %i",
          ":column %i",
          ":signature %s",
          ":help %s)"])
 (quote (tplMakeName name inst)) 
 (tplInstanceOf inst)
 (tplMaybe (fmap (quote . get identStr) parent)) 
 (quote file)
 line
 column
 (tplMaybe (fmap quote sig)) 
 (tplMaybe (fmap quote qh)) 
           
elisp :: FilePath -> Result -> String
elisp file (mod, imps, exps, db) = printf 
                                   (unlines ["(setq *hie-load* (make-hiemod :name %s",
                                             ":defs %s",
                                             ":imports %s",
                                             ":exports %s))"])
                                   (if (null mod) then "nil" else (quote mod))
                                   (tplList $ map (tplLocalDef file) $ Map.toList db)
                                   (tplList $ map tplImport imps)
                                   (tplList $ map tplExport exps)

-- Driver ----------------------------------------------------------------------

usage = putStrLn "Usage: hie file-to-jump-to (reads file from stdin; writes to stdout)"


runCPP :: Bool -> String -> IO String
runCPP unlit contents = CPP.runCpphs cppOpts "" contents
  where
    cppOpts = CPP.defaultCpphsOptions { 
      CPP.defines = [("__GLASGOW_HASKELL__", "0"),
                     ("SIZEOF_HSWORD", "4"),
                     ("FLT_RADIX", "2"),
                     ("WORD_SIZE_IN_BITS", "32"),
                     ("HAVE_KQUEUE", "1"),
                     ("CALLCONV", "ccall")], 
      CPP.boolopts = CPP.defaultBoolOptions { 
         CPP.hashline = False,
         CPP.warnings = False,
         CPP.literate = unlit
         } 
      }
              
    
sub regex repl s = subRegex (mkRegex regex) s repl
  

fixBugsHack :: String -> String
fixBugsHack = --sub "\\(# " "(" . -- stupid bug in haskell-src-exts 
--               sub " #\\)" ")" . -- stupid bug in haskell-src-exts 
--               sub "0x[A-Fa-f0-9]+#" "0" .  -- stupid bug in haskell-src-exts
--               sub "SPECIALISE \\[[0-9]\\]" "SPECIALISE" . -- stupid bug in haskell-src-exts
--               sub "'\\\\x[A-Fa-f0-9]+'" "'k'" . -- stupid bug in haskell-src-exts
                sub "^[A-Z0-9_]+\\(.*\\)[ \t]*$" "" -- try to guess cpp macros. this isn't even valid haskell syntax

main :: IO ()
main = do 
  files <- getArgs
  case files of 
    [path] -> do
      contents <- getContents   >>= runCPP (".lhs" `isSuffixOf` path) >>= (return . fixBugsHack)
      --let contents' = fixBugsHack contents
      --putStrLn contents'
      case createTags path contents of 
        Left res -> putStrLn (elisp path res) >>
          exitSuccess
        Right s -> hPutStrLn stderr s >> exitFailure
    _ -> usage
  
                