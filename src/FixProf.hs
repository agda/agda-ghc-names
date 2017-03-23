-- agda-ghc-names/FixProf.hs
-- Copyright 2015 by Wolfram Kahl
-- Licensed under the same terms as Agda

-- Given a mapping |HsModname -> HsIdent -> Maybe AgdaIdent|,
-- transform a *.prof file into *.agdaIdents.prof


module FixProf where

-- import ResolveHsNames (apply2M, getResolveHsNamesMap)
import Data.Char (isSpace)
import Data.List (stripPrefix)
-- import Control.Monad (liftM)
-- import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

splitFirstDot :: String -> (String, Maybe String)
splitFirstDot s = case span ('.' /=) s of
  (hd, []) -> (hd, Nothing)
  (hd, _ : tl) -> (hd, Just tl)

resolveCC0 :: (String -> String -> Maybe String) -> String -> String -> (String, Maybe String)
resolveCC0 resolve cc hmod = case splitFirstDot cc of
  (hsIdent, mlocalCC) -> case resolve hmod hsIdent of
    Nothing -> (hsIdent, mlocalCC)
    Just agdaIdent -> (agdaIdent, mlocalCC)

resolveRawCC :: (String -> String -> Maybe String) -> String -> String -> String
resolveRawCC resolve cc hmod = case resolveCC0 resolve cc hmod of
  (agdaIdent, m) -> case m of
    Nothing -> agdaIdent
    Just localCC -> agdaIdent ++ '.' : localCC

resolveCC :: (String -> String -> Maybe String) -> String -> String -> String
resolveCC resolve cc hmod = case stripPrefix "CAF:" cc of
  Nothing -> resolveRawCC resolve cc hmod
  Just cc' -> "CAF:" ++ resolveRawCC resolve cc' hmod

-- If |checkPrefix pre s == Just (k, s')|,
-- then |k| is the width of the ``|pre|'' column,
-- and |s'| contains the remaining columns.
checkPrefix :: String -> String -> Maybe (Int, String)
checkPrefix pre s = case stripPrefix pre s of
  Just s' -> case span (' ' ==) s' of
    (spaces, s'') -> Just (length pre + length spaces, s'')
  _ -> Nothing

-- If |checkSepLine s == Just (ccWidth, modWidth, mSrcWidth)|,
-- then these are the widths of the first two columns
-- (assuming that the "no." is always indented three positions),
-- respectively three columns with GHC-8, where "no." is not indented anymore.
checkSepLine :: String -> Maybe (Int, Int, Maybe Int)
checkSepLine s = case checkPrefix "COST CENTRE" s of
  Just (ccWidth, s') -> case checkPrefix "MODULE" s' of
      Just (modWidth0, s'') -> let
          (modWidth, mSrcWidth) = case stripPrefix "no." s'' of
            Just _ -> (modWidth0 - 3, Nothing)
            Nothing -> case checkPrefix "SRC" s'' of
              Just (srcWidth, s''') -> (modWidth0, Just srcWidth)
              Nothing -> (modWidth0, Nothing)
        in Just (ccWidth, modWidth, mSrcWidth)
      Nothing -> error $ "checkSepLine: unexpected line: " ++ show s
  _ -> Nothing

-- In the body of *.prof files,
-- the COST CENTRE column is indented by call depth,
-- which we need to preserve.
-- |firstWord s| returns the pair consisting of the
-- initial spaces and the first word in |s|:
firstWord :: String -> (String, String)
firstWord s = case span isSpace s of
  (spaces, s') -> (spaces, takeWhile (not . isSpace) s')

-- |splitLine ccWidth modWidth s = ((indent, ccName), modName, rest)|
splitLine :: Int -> Int -> Maybe Int -> String -> ((String, String), (String, String), String)
splitLine ccWidth modWidth mSrcWidth s = case splitAt ccWidth s of
  (cc0, s') -> case splitAt modWidth s' of
    (mod0, s'') ->  let
        hmod = head (words mod0)
        (src, s''') = case mSrcWidth of
          Nothing -> ("", s'')
          Just srcWidth -> splitAt srcWidth s''
       in (firstWord cc0, (hmod, src), s''')

updateLine :: (String -> String -> Maybe String) -> Int -> Int -> Bool -> Maybe Int -> Bool -> String -> (String, String)
updateLine resolve ccWidth modWidth keepMod mSrcWidth keepSrc s
   = case splitLine ccWidth modWidth mSrcWidth s of
  ((indent, cc), (hmod, src), s') -> (,) (indent ++ resolveCC resolve cc hmod)
     (let s'' = if keepMod then spaceOut modWidth hmod s' else s'
     in if keepSrc then maybe (++) spaceOut mSrcWidth src s'' else s'')

spaceOut :: Int -> String -> String -> String
spaceOut w s1 s2 = take w (s1 ++ repeat ' ') ++ ' ' : ' ' : s2

updateLines :: (String -> String -> Maybe String) -> Int -> Int -> Bool -> Maybe Int -> Bool -> [String] -> ((Int, [String]), [String])
updateLines resolve ccWidth modWidth keepMod mSrcWidth keepSrc ls = case h id ls of
    (ps, ls') -> let
        w = maximum (map (length . fst) ps)
        render (cc, rest) = spaceOut w cc rest
      in ((w, map render ps), ls')
  where
    h acc [] = (acc [],[])
    h acc ([] : ss) = (acc [], ss)
    h acc (s : ss) = h (acc . (updateLine resolve ccWidth modWidth keepMod mSrcWidth keepSrc s :)) ss

updateProf :: (String -> String -> Maybe String) -> Bool -> Bool -> [String] -> [String]
updateProf resolve keepMod keepSrc [] = []
updateProf resolve keepMod keepSrc (s : ss) = case checkSepLine s of
  Nothing -> s : updateProf resolve keepMod keepSrc ss
  Just (ccWidth, modWidth, mSrcWidth) -> let
    ((w, new), rest) = updateLines resolve ccWidth modWidth keepMod mSrcWidth keepSrc (drop 1 ss)
    s' = drop ccWidth s
    s'' = if keepMod then s' else drop modWidth s'
    s''' = if keepSrc then s'' else maybe id drop mSrcWidth s''
   in  spaceOut w "COST CENTRE" s'''
   :   []
   :   new
   ++  []
   :   updateProf resolve keepMod keepSrc rest

updateProfFile  :: IO ()
                -> (String -> String -> Maybe String)
                -> Bool -> Bool -> FilePath -> IO ()
updateProfFile usage resolve keepMod keepSrc path
  = case stripPrefix (reverse ".prof") (reverse path) of
  Nothing -> usage
  Just revBasename -> let
      path' = reverse revBasename ++ ".agdaIdents"
            ++  ( (if keepMod then ("_MOD" ++) else id)
                $ (if keepSrc then ("_SRC" ++) else id)
                $ ".prof"
                )
    in do
    s <- readFile path
    hPutStrLn stderr $ "read " ++ path
    writeFile path' . unlines . updateProf resolve keepMod keepSrc $ lines s
    hPutStrLn stderr $ "wrote " ++ path'
