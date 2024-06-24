{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (Pretty(..)) where 
import Data.List (intercalate)

class Pretty a where
  pretty :: a -> String
  default pretty :: Show a => a -> String
  pretty = show

instance Pretty a => Pretty [a] where
  pretty as = "[" ++ intercalate "," (map pretty as) ++ "]"

instance {-# OVERLAPS #-} Pretty String where
  pretty :: String -> String
  pretty s = s

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty :: (Pretty a, Pretty b) => (a, b) -> String
  pretty (a, b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = "(" ++ pretty a ++ ", " ++ pretty b ++ ", " ++ pretty c ++ ")"

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (a, b, c, d) = "(" ++ pretty a ++ ", " ++ pretty b ++ ", " ++ pretty c ++ ", " ++ pretty d ++ ")"