
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module KeyValueMap where 
import Data.Data (Data, Typeable)

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

selectAssocVal :: String -> ValueKVM -> Maybe ValueKVM
selectAssocVal s (MapVM kvm) = case filter (\(k,_) -> k == s) kvm of
  [kvp] -> Just (snd kvp)
  [] -> Nothing
  _ -> error ("several possible choices for " ++ show s)
selectAssocVal s _ = Nothing
