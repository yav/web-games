module Common.Enum ( EnumText(..), declareEnumText) where

import Control.Applicative((<|>))
import Data.Text(Text)
import qualified Data.Text as Text
import Data.List(partition)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype.TyVarBndr
import Control.Monad(forM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS


--------------------------------------------------------------------------------

class EnumText a where
  enumToText    :: a -> Text
  enumFromText  :: Text -> Maybe a

enumTextToJSKey :: EnumText a => JS.ToJSONKeyFunction a
enumTextToJSKey = JS.toJSONKeyText enumToText

enumTextFromJSKey :: EnumText a => JS.FromJSONKeyFunction a
enumTextFromJSKey = JS.FromJSONKeyTextParser \t ->
  case enumFromText t of
    Just a  -> pure a
    Nothing -> fail "failed parse JSON key"


--------------------------------------------------------------------------------
sep :: Text
sep = "."

mkQual :: Text -> Text -> Text
mkQual a b = a <> sep <> b

mkQualS :: String -> String -> String
mkQualS a b = a ++ Text.unpack sep ++ b


declareEnumText :: Name -> Q [Dec]
declareEnumText tcon =
  do info <- repsOfTC tcon
     [d| instance EnumText $(pure (tiTy info)) where
            enumToText x   = $(toTextDef   info [| x |])
            enumFromText x = $(fromTextDef info [| x |])

         instance JS.ToJSON $(pure (tiTy info)) where
           toJSON = JS.toJSON . enumToText

         instance JS.FromJSON $(pure (tiTy info)) where
           parseJSON = JS.withText nameStr \x ->
                       case enumFromText x of
                         Just a  -> pure a
                         Nothing -> fail "Invalid value"

         instance JS.ToJSONKey $(pure (tiTy info)) where
           toJSONKey = enumTextToJSKey

         instance JS.FromJSONKey $(pure (tiTy info)) where
           fromJSONKey = enumTextFromJSKey
       |]
  where
  nameStr = nameBase tcon
  v = mkName "v"

  alt p e = match p (normalB e) []

  grd x e =
    let q = mkQualS x ""
    in match (varP v)
             (guardedB [normalGE [| q `Text.isPrefixOf` $(varE v) |] e]) []

  toTextDef info x =
    caseE x
     $ [ alt (conP n []) [| s |]
       | n <- tiDirect info
       , let s = nameBase n
       ] ++
       [ alt (conP n [ varP v ])
             [| mkQual $[| s |] (enumToText $(varE v)) |]
       | n <- tiSubQual info
       , let s = nameBase n
       ] ++
       [ alt (conP n [ varP v ]) [| enumToText $(varE v) |]
       | n <- tiSubDirect info
       ]

  fromTextDef info x =
    caseE x
      $ [ alt (litP (stringL s)) [| Just $(conE n) |]
        | n <- tiDirect info
        , let s = nameBase n
        ] ++
        [ grd s [| $(conE n) <$> enumFromText (Text.drop $[| l |] $(varE v))|]
        | n <- tiSubQual info
        , let s = nameBase n
              l = length s + Text.length sep
        ] ++
        [ alt [p| _ |]
          let opts = [ [| $(conE n) <$> enumFromText $x |]
                     | n <- tiSubDirect info
                     ]
              jn l r = [| $l <|> $r |]
          in case opts of
               [] -> [| Nothing |]
               _  -> foldr1 jn opts
        ]

data TInfo = TInfo
  { tiTy        :: Type
  , tiVals      :: [String]
  , tiDirect    :: [Name]
  , tiSubQual   :: [Name]
  , tiSubDirect :: [Name]
  }


repsOfTC :: Name -> Q TInfo
repsOfTC tcon =
  do info <- reify tcon
     (ty,cons) <-
        case info of
          TyConI dec -> consOf dec
          _          -> unsupported "thing"

     subNames <- forM cons \(c,ns) ->
                   case ns of
                     [] -> pure (Left c)
                     [n] -> do subs <- repsOfType n
                               pure (Right (c,subs))
                     _ -> unsupported "multiple fields"

     let ambig = Map.keysSet
               $ Map.filter (> 1)
               $ Map.fromListWith (+)
                    [ ent
                    | it <- subNames
                    , ent <- case it of
                               Left c -> [(nameBase c,1 :: Int)]
                               Right (_,sub) -> [ (n,1) | n <- tiVals sub ]
                    ]

         blank = TInfo { tiTy = ty
                       , tiVals = []
                       , tiDirect = []
                       , tiSubQual = []
                       , tiSubDirect = []
                       }
         addC con i =
           case con of
             Left c -> i { tiVals = nameBase c : tiVals i
                         , tiDirect = c : tiDirect i
                         }
             Right (c,sub) ->
                let (q,d) = partition (`Set.member` ambig) (tiVals sub)
                    qs    = [ mkQualS (nameBase c) x | x <- q ]
                in i { tiVals = d ++ qs ++ tiVals i
                     , tiSubQual = [ c | not (null qs) ] ++ tiSubQual i
                     , tiSubDirect = [ c | not (null d) ] ++ tiSubDirect i
                     }

     pure (foldr addC blank subNames)

repsOfType :: Type -> Q TInfo
repsOfType ty = repsOfTC =<< typeConOf ty

unsupported :: String -> Q a
unsupported msg = fail ("EnumText: " ++ msg)

typeConOf :: Type -> Q Name
typeConOf ty =
  case ty of
    AppT t1 _         -> typeConOf t1
    AppKindT t1 _     -> typeConOf t1
    SigT t1 _         -> typeConOf t1
    ParensT t1        -> typeConOf t1

    ConT x            -> pure x
    InfixT _ x _      -> pure x

    _                 -> unsupported "type"

normalCon :: Con -> Q (Name, [Type])
normalCon con =
  case con of
    NormalC c ts      -> pure (c, map unbang ts)
    RecC c ts         -> pure (c, map unbangv ts)
    _                 -> unsupported "constructor"
  where
  unbang (_,t) = t
  unbangv (_,_,t) = t


consOf :: Dec -> Q (Type,[(Name,[Type])])
consOf dec =
  case dec of
    DataD _ x ts _ cs _        -> addTy x ts <$> mapM normalCon cs
    NewtypeD _ x ts _ c _      -> addTy x ts . pure <$> normalCon c
    _                          -> unsupported "declaration"

  where
  paramT = VarT . tvName
  addTy d as y = (foldl AppT (ConT d) (map paramT as), y)
