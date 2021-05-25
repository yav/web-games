module Income(incomeFromStatus,statusAfterLoan) where

incomeFromStatus :: Int -> Int
incomeFromStatus i = incomeBase stage + 1 + div (i - stageStart stage - 1) stage
  where stage = incomeStage i

statusAfterLoan :: Int -> Int
statusAfterLoan = fromIncome . subtract 3 . incomeFromStatus

incomeBase :: Int -> Int
incomeBase stage = 10 * (stage - 2)

stageStart :: Int -> Int
stageStart stage
  | stage == 1  = 0
  | stage == 2  = 10
  | stage == 3  = 30
  | otherwise   = 60

incomeStage :: Int -> Int
incomeStage i = head [ s | s <- [4,3,2,1], stageStart s <= i ]

fromIncome :: Int -> Int
fromIncome i = stage * (i - base) + stageStart stage
  where
  (stage,base) = head [ (s,b) | s <- [ 4,3,2,1 ], let b = incomeBase s, b <= i ]



