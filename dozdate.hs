import System( getArgs )

translateBase :: Integer -> String
translateBase 10 = "χ"
translateBase 11 = "Ɛ"
translateBase x = show x

main = do
	args <- getArgs
	let hour = read $ args!!0 :: Integer
	let min = read $ args!!1 :: Integer
	let sec = read $ args!!2 :: Integer
	let mil = (read $ args!!3 :: Integer) `div` 1000000
	let time1 = (*) 144 . (+) mil . (*) 1000 . (+) sec $ (+) ((*) hour 3600) ((*) min 60)
	let time2 = (/) (fromIntegral time1) 25
	let time = floor $ (/) time2 1000
	let dozHour1 = translateBase $ floor $ (/) (fromIntegral time) 20736
	let dozHour2 = translateBase $ mod (floor ((/) (fromIntegral time) 20736)) 12
	let dozMin = div (mod time 20736) 144
	let dozMin1 = translateBase $ (floor $ (fromIntegral dozMin) / 12)
	let dozMin2 = translateBase $ mod dozMin 12
	let dozSec = floor $ fromIntegral $ mod time 144
	let sec1 = translateBase $ floor $ (/) (fromIntegral dozSec) 12
	let sec2 = translateBase $ mod dozSec 12
	let dozSec1 = translateBase $ floor $ (/) (fromIntegral sec) 10
	let dozSec2 = translateBase $ mod sec 10
	
	putStrLn $	(if hour < 12 
				then "0" ++ (dozHour1) else "1" ++ (dozHour2))
				++ ":" ++ (dozMin1) ++ (dozMin2) ++ ":" ++ (sec1) ++ (sec2)

