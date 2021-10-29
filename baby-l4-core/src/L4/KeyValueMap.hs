
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module L4.KeyValueMap where
import Data.Data (Data, Typeable)
import Data.Maybe (fromMaybe, isJust)

type KeyKVM = String
data ValueKVM
  = IdVM String
  | BoolVM Bool
  | IntVM Integer
  | MapVM KVMap
  deriving (Eq, Ord, Show, Read, Data, Typeable)
type KVMap = [(KeyKVM, ValueKVM)]

selectOneOfInstr :: [String] -> ValueKVM ->  String
selectOneOfInstr chs (IdVM s) =
  if s `elem` chs
  then s
  else error ("choose exactly one of " ++ show chs)
selectOneOfInstr chs (MapVM kvm) = case filter (\(k,_) -> k `elem` chs) kvm of
  [kvp] -> fst kvp
  _ -> error ("choose exactly one of " ++ show chs)
selectOneOfInstr chs _ = error ("incorrect instruction for choice among " ++ show chs)


-- TODO: instead of making these checks here, some elementary properties of KVM should
-- be verified during type checking, such as uniqueness of keys
-- --> eventually define by getAssocOfPathMap
-- selectAssocOfMap :: KeyKVM -> KVMap -> Maybe ValueKVM
-- selectAssocOfMap s kvm = case filter (\(k,_) -> k == s) kvm of
--   [kvp] -> Just (snd kvp)
--   [] -> Nothing
--   _ -> error ("several possible choices for " ++ show s)

-- --> eventually define by getAssocOfPathValue
-- selectAssocOfValue :: KeyKVM -> ValueKVM -> Maybe ValueKVM
-- selectAssocOfValue k (MapVM kvm) = selectAssocOfMap k kvm
-- selectAssocOfValue _ _ = Nothing

hasKeyKVMap :: KeyKVM -> KVMap -> Bool
hasKeyKVMap k m = k `elem` (map fst m)

-- hasPathMap ks kvm: there is a value associated with the path ks in map kvm
hasPathMap :: [KeyKVM] -> KVMap -> Bool
--hasPathMap [] _ = True
--hasPathMap (k : ks) m = maybe False (hasPathValue ks) (selectAssocOfMap k m)
hasPathMap ks m = isJust (getAssocOfPathMap ks m)

hasPathValue :: [KeyKVM] -> ValueKVM -> Bool
--hasPathValue [] _ = True
--hasPathValue ks (MapVM kvm) = hasPathMap ks kvm
--hasPathValue _ _ = False
hasPathValue ks v = isJust (getAssocOfPathValue ks v)

removeKeyFromMap :: KeyKVM -> KVMap -> KVMap
removeKeyFromMap s = filter (\(k,_) -> k /= s)

stringListAsKVMap :: [String] -> KVMap
stringListAsKVMap = map (\s -> (s, MapVM []))


getAssocOfPathValue :: [KeyKVM] -> ValueKVM -> Maybe ValueKVM
getAssocOfPathValue [] v = Just v
getAssocOfPathValue ks (MapVM kvm) = getAssocOfPathMap ks kvm
getAssocOfPathValue _ _ = Nothing

getAssocOfPathMap :: [KeyKVM] -> KVMap -> Maybe ValueKVM
getAssocOfPathMap (k:ks) m = getAssocOfPathValue ks =<< lookup k m
getAssocOfPathMap _ _ = Nothing


putAssocOfPathValue :: [KeyKVM] -> ValueKVM -> ValueKVM -> ValueKVM
putAssocOfPathValue [] v _ = v
putAssocOfPathValue ks v (MapVM kvm) = MapVM (putAssocOfPathMap ks v kvm)
putAssocOfPathValue ks v _ = MapVM (putAssocOfPathMap ks v [])

-- putAssocOfPathMap ks v m: replace map at position / path ks of map m with value v
-- If the position ks does not exist in m, create it
putAssocOfPathMap :: [KeyKVM] -> ValueKVM -> KVMap -> KVMap
putAssocOfPathMap [] _ _ = error "path should not be empty"
putAssocOfPathMap (k : ks) v m =
  (k, putAssocOfPathValue ks v (fromMaybe (MapVM []) (getAssocOfPathMap [k] m))) : removeKeyFromMap k m


mymap :: ValueKVM
mymap = MapVM [("valid",MapVM []),("procs",MapVM [("AutA",MapVM []),("AutC",MapVM [])]),("config",MapVM [("loglevel",IntVM 0),("logic",IdVM "AUFLIRA")])]

-- >>> hasPathValue ["config", "logic"] mymap
-- True
