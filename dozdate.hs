import System( getArgs )
import Data.Functor
import Numeric

translateBase x = buffer $ showIntAtBase 12 (\ y -> flip (!!) y $ ['0' .. '9'] ++ ['Ӿ', 'Ɛ'] ) x ""

buffer [] = "00"
buffer (x:[]) = '0':x:[]
buffer xs = xs

main = do
	args@( hour : min : sec : _ ) <- map read <$> getArgs :: IO [Integer]
	let mil = flip div 1000000 $ args !! 3

	let time1 = (*) 144 . (+) mil . (*) 1000 . (+) sec $ (+) ((*) hour 3600) ((*) min 60)
	let time = floor $ flip (/) 1000 $ (/) (fromIntegral time1) 25

	let dozMin = div (mod time 20736) 144
	let dozSec = floor $ fromIntegral $ mod time 144
	
	putStrLn $ (translateBase hour) ++ ":" ++ translateBase dozMin ++ ":" ++ translateBase dozSec

