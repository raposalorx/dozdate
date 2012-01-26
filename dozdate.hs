import System( getArgs )
import Data.Functor
import Numeric

trans i = prep.buffer.translate
  where translate = showIntAtBase 12 ((['0'..'9']++['Ӿ', 'Ɛ'])!!) i
        buffer (x:[]) = '0':[x]
        buffer xs@(x:':':_) = '0':xs
        buffer xs = xs
        prep = (':':)

main = do
	args@( hour : min : sec : _ ) <- map read <$> getArgs :: IO [Integer]
	let mil = (`div` 1000000) $ args !! 3

	let time = floor.(/1000).(/25).fromIntegral.(144*).(mil+).(1000*).(sec+) $ (hour*3600)+(min*60)

	let dozMin = (`div`144).(`mod`20736) $ time
	let dozSec = floor.fromIntegral.(`mod`144) $ time
	
	putStrLn $ drop 1 $ (trans hour).(trans dozMin).(trans dozSec) $ ""
