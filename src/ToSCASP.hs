module ToSCASP where
import Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)
import Syntax

createSCasp :: (Show ct, Show et) => Program ct et -> IO ()
createSCasp p = putDoc $ (showSC p <> PP.line)


class SCasp x where
  showSC :: x -> Doc ann
  showSClist :: [x] -> Doc ann
  showSClist xs = encloseSep lbracket rbracket comma (showSC <$> xs)

instance SCasp (Program ct et) where
  showSC _ = pretty "This is a program"