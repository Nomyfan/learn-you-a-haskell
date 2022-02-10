module Module1
(
  addUpPolynomials
) where

import           Data.List
import           Geometry.Cube


-- List实用API
-- intersperse
-- intercalate
-- transpose

-- 对多项式3x^2+5x+9, 10x^3+9, 8x^3+5x^2+x-1合并，可以这样计算
-- map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
addUpPolynomials :: [[Integer]] -> [Integer]
addUpPolynomials = map sum . transpose

-- concat
-- concatMap
-- and, or, any, all
-- iterate
-- splitAt
-- takeWhile
-- dropWhile
-- span, break
-- sort
-- group, partition
-- isInfixOf, isPrefixOf, isSuffixOf
-- elem, notElem, find, elemIndex, elemIndices, findIndex
-- lines, unlines, words, unwords
-- nub
-- delete

-- delete 'h' . delete 'h' $ "hey there ghang!"

-- \\

-- "Im a big baby" \\ "big"
-- delete 'b' . delete 'i' . delete 'g' $ "Im a big baby"
