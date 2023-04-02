-- | Generate a JavaScript consumer for Haskell sum types.
module Common.CallJS (jsHandlers) where

import Data.List(intercalate)
import Language.Haskell.TH

jsHandlers :: [Name] -> Q Exp
jsHandlers xs =
  do es <- mapM jsHandler xs
     pure (LitE (StringL (unlines ("// Start" : es))))

jsHandler :: Name -> Q String
jsHandler nm =
  do info <- reify nm
     case info of
       TyConI (DataD _ _ _ _ cs _)
         | Just fun <- jsData nm cs -> pure fun
       _ -> fail "jsHandler: unsupported declaration"

jsData :: Name -> [Con] -> Maybe String
jsData nm cons = defFun <$> mapM jsCon cons
  where
  defFun cs =
     unlines $
       [ "function hs" <> nameBase nm <> "(self) { return function (msg) {"
       , "  switch(msg.tag) {" ]
       ++ cs ++
       [ "  }"
       , "}}"
       ]


jsCon :: Con -> Maybe String
jsCon con =
  case con of
    NormalC c ts   -> doCon c ts
    InfixC b1 c b2 -> doCon c [b1,b2]
    _ -> Nothing

  where
  doCon c ts =
    Just (unlines [ "    case " <> show (nameBase c) <> ":"
                  , "      return self." <> nameBase c <> "(" <> params <> ")"
                  ])
    where
    params = case length ts of
               0 -> ""
               1 -> "msg.contents"
               n -> intercalate "," [ "msg.contents[" ++ show i ++ "]"
                                    | i <- take n [ 0 :: Int .. ] ]
 
