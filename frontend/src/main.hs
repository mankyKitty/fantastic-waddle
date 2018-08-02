import Reflex.Dom
import Frontend

main :: IO ()
main = do
  sGen <- Rnd.getStdGen
  mainWidgetWithHead headWidget (body sGen)
