-- | Utilities for working with fields (simple "lenses")
module Common.Field where

import Data.Map(Map)
import qualified Data.Map as Map
import Language.Haskell.TH
import Language.Haskell.TH.Datatype.TyVarBndr

-- | Describes a field in @r@ of type @a@
data Field r a = Field
  { getField :: r -> a
  , setField :: a -> r -> r
  }

-- | A synthetic field for direct access to fields of nested compoinents.
(.>) :: Field r a -> Field a b -> Field r b
x .> y = Field
           { getField = getField y . getField x
           , setField = updField x . setField y
           }

viewField :: Field r a -> (a -> b) -> r -> b
viewField f v = v . getField f

infixr 4 `updField`

-- | Update a field
updField :: Field r a -> (a -> a) -> r -> r
updField x f = \r -> setField x (f (getField x r)) r

updField' :: Field r a -> (a -> (b,a)) -> r -> (b,r)
updField' x f = \r -> let (b,a) = f (getField x r)
                      in (b, setField x a r)

-- | Effectful update to a field
traverseField :: Functor m => Field r a -> (a -> m a) -> r -> m r
traverseField x f = \r -> (\v -> setField x v r) <$> f (getField x r)


-- | A field corresponding to a key in a map.
-- Assumess that the key is in the map.
mapAt :: Ord a => a -> Field (Map a b) b
mapAt a = Field
  { getField = \m -> m Map.! a
  , setField = \v -> Map.insert a v
  }

-- | A field corresponding to a key in a map.
mapAtMaybe :: Ord a => a -> Field (Map a b) (Maybe b)
mapAtMaybe a = Field
  { getField = Map.lookup a
  , setField = \v -> case v of
                       Nothing -> Map.delete a
                       Just b  -> Map.insert a b
  }

-- | A field corresponding to the given index in a list.
-- Assumes tha the index is in the list.
-- Accessing the field is linear in the lenght of the list,
-- so this should probably be used only with short lists.
listAt :: Int -> Field [a] a
listAt n = Field
  { getField = \xs -> case drop n xs of
                        a : _ -> a
                        _     -> error "listAt: index out of bounds"
  , setField = \v xs -> case splitAt n xs of
                          (as,_:bs) -> as ++ v : bs
                          _         -> xs
  }


-- | Declare fields for a record type.  The fields are derived from
-- the labels in the record.  Fields are derived only for records with
-- name that start with @_@, and the generate field for @_name@ is name.
declareFields :: Name -> Q [Dec]
declareFields tyName =
  do info <- reify tyName
     case info of
       TyConI (DataD [] d as _ [RecC _ fs]  _) ->
          do let paramT = VarT . tvName
                 valT = foldl AppT (ConT d) (map paramT as)
             concat <$> mapM (declareField as valT) fs
       _ -> fail $ unlines
                    [ "`declareField` works only on:"
                    , "  * data declarations"
                    , "  * with no context"
                    , "  * a single record constructor"
                    ]
  where
  declareField _as ty (f,_,t) =
    case nameBase f of
      '_' : nmStr ->
        do let nm = mkName nmStr
               sig = SigD nm (ConT ''Field `AppT` ty `AppT` t)
               v = mkName "v"
               r = mkName "r"
               getF = VarE f
               setF = LamE [VarP v]
                    $ LamE [VarP r]
                    $ RecUpdE (VarE r) [(f,VarE v)]

           def <- [| Field { getField = $(pure getF),
                             setField = $(pure setF) } |]
           pure [ sig, FunD nm [Clause [] (NormalB def) []] ]
      _ -> pure []
