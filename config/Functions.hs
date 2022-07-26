-- drummachine init ::

let 
    drumMachine name ps = stack (map (\ x -> (# s (name ++| (extractS "s" (x)))) $ x) ps)
    drumFrom name drum = s (name ++| drum)
    drumM = drumMachine
    drumF = drumFrom

-- functions ::
let
    bpm a = setcps(a/60/4)
    robo = binaryN 16 "<730950 35928 9852908 379528>"
    gtfo p = (const $ s "~") p
    twoglit = (while (robo) ((ply "~ [2 1] ~ [3 2]").(# speed "0.5")))
    twoglit' = ((ply "<2 4> 3").(# coarse (every 2 rev "4 .. 8")))
    roll p = stutWith 2 (0.125*3) id $ p
    rollspeed p = stutWith 2 (1/4) (|* speed (range 0.75 1.5 $ sine)) $ p
    rollvowel p = stutWith 2 (1/4) (|* vowel "a ~") $ p
    roll' p = stutWith 2 (0.0625*3) id $ p
    rollSlow howOften p = sometimesBy howOften (stutWith 2 (0.125*3) id) $ p
    rollFast howOften p = sometimesBy howOften (stutWith 2 (0.0625*3/2) id) $ p
    shiftB a b = whenmod 16 4 (while a (b <~))
    shiftF a b = whenmod 16 4 (while a (b ~>))
    mute = const silence
    warpF = whenmod 16 15 (rev . striate 8)
    shift' n p = (n <~) $ p
    shift p = (1 <~) $ p
    bonk p = (# delay (range 0.1 0.3 $ shift' 1881 $ rand)) $ (# delayt (shift' 1882 $ (choose [(1/2048),(1/1024)..(1/32)]))) $ (# delayfb (range 0.7 0.99 $ shift' 1883 $ rand)) $ p
