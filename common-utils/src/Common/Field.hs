{-# Language TemplateHaskell #-}
module Common.Field where

import Data.Map(Map)
import qualified Data.Map as Map
import Language.Haskell.TH

data Field r a = Field
  { getField :: r -> a
  , setField :: a -> r -> r
  }

(.>) :: Field r a -> Field a b -> Field r b
x .> y = Field
           { getField = getField y . getField x
           , setField = updField x . setField y
           }

infixr 4 `updField`

updField :: Field r a -> (a -> a) -> r -> r
updField x f = \r -> setField x (f (getField x r)) r


mapAt :: Ord a => a -> Field (Map a b) b
mapAt a = Field
  { getField = \m -> m Map.! a
  , setField = \v -> Map.insert a v
  }

mapAtMaybe :: Ord a => a -> Field (Map a b) (Maybe b)
mapAtMaybe a = Field
  { getField = Map.lookup a
  , setField = \v -> case v of
                       Nothing -> Map.delete a
                       Just b  -> Map.insert a b
  }

listAt :: Int -> Field [a] a
listAt n = Field
  { getField = \xs -> case drop n xs of
                        a : _ -> a
                        _     -> error "listAt: index out of bounds"
  , setField = \v xs -> case splitAt n xs of
                          (as,_:bs) -> as ++ v : bs
                          _         -> xs
  }


declareFields :: Name -> Q [Dec]
declareFields tyName =
  do info <- reify tyName
     case info of
       TyConI (DataD [] d as _ [RecC _ fs]  _) ->
          do let paramT a = case a of
                              PlainTV x -> VarT x
                              KindedTV x _ -> VarT x
                 valT = foldl AppT (ConT d) (map paramT as)
             concat <$> mapM (declareField as valT) fs
       _ -> fail $ unlines
                    [ "`declareField` works only on:"
                    , "  * data declarations"
                    , "  * with no context"
                    , "  * a single record constructor"
                    ]
  where
  declareField as ty (f,_,t) =
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
