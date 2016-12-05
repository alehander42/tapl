module Main
where

data Value = VlTrue |
			 VlFalse |
			 VlZero |
			 VlS Value |
			 VlP Value |
			 VlIsZero Value |
			 VlIf Value Value Value |
			 VlError deriving (Show)


isNumeric :: Value -> Bool
isNumeric VlZero = True
isNumeric (VlS v)  = isNumeric v
isNumeric _ 	 = False

isValid :: Value -> Bool
isValid VlFalse  = True
isValid VlTrue 	 = True
isValid v 		 = isNumeric v

eval1 :: Value ->   Value
eval1 (VlIsZero VlZero)  = VlTrue
eval1 (VlIsZero (VlS v)) = VlFalse
eval1 (VlP VlZero) = VlZero
eval1 (VlP VlError) = VlError  
eval1 (VlP v) = if isNumeric(v) then VlS (eval1 v) else VlError
eval1 (VlS (VlP v)) = if isNumeric(v) then v else VlError 
eval1 (VlS VlError) = VlError
eval1 (VlS v) = if isNumeric(v) then VlP (eval1 v) else VlError
eval1 (VlIf VlTrue v1 v2) = v1
eval1 (VlIf VlFalse v1 v2) = v2
eval1 (VlIf VlError _ _) = VlError 
eval1 (VlIf value v1 v2) = VlIf (eval1 value) v1 v2
eval1 VlError = VlError
eval1 _ = VlError

main :: IO ()
main = do
  a <- return (eval1 (VlS (VlS (VlS (VlS VlZero)))))
  print a