 include div_mul.asm

 rem Support Black and White Monitors and hide score based on new color setting.
 if switchbw then COLUBK = $00:COLUPF = $0E else COLUBK= $0E:COLUPF= $80
 if switchbw then scorecolor = $00 else scorecolor = $0E



 dim timer = c



introloop
 drawscreen
 playfield:
 ................................
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 ................................
 XX.X...XXX.XXX.X...X.XXX.XXX..XX
 ...X...X...X.X.XX..X.X...X.X....
 XX.X...XX..XXX.X.X.X.XX..XXX..XX
 ...X...X...XX..X..XX.X...XX.....
 XX.XXX.XXX.X.X.X...X.XXX.X.X..XX
 ................................
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 ................................
end
 timer = timer + 1
 if timer = 128 then goto introloop2
 goto introloop

introloop2
 timer = 0
 drawscreen
 playfield:
 ................................
 ................................
 .XX.XX..XX.XX.XX...X...X..X..X.X
 .X..X.X.X..X..X.X..X...X.X.X.X.X
 .XX.XX..XX.XX.X.X..X...X.XXX..X.
 ..X.X...X..X..X.X..X.X.X.X.X..X.
 .XX.X...XX.XX.XX....X.X..X.X..X.
 ................................
 XX..XX..XX..XX..XX..XX..XX..XX..
 ..XX..XX..XX..XX..XX..XX..XX..XX
 ................................
end
 if switchreset || joy0fire then goto gameinit
 goto introloop2







gameinit
 rem Detect B&W Switch and change colors of background, playfield, and score color respectively.
 if switchbw then COLUBK = $00:COLUPF = $0E else COLUBK= $05:COLUPF= $1E
 if switchbw then scorecolor = $0E else scorecolor = $40


 AUDV0 = 0
 dim mph = a
 dim badguymph = b
 mph = 0
 badguymph = 0
 player0x = 95
 player0y = 70
 player1x = 50
 player1y = 20
 score = 0

 playfield:
 XX.X.X.........X..........X.XXXX
 X.X..X....................X.X..X
 XXXX.X.........X..........X.X..X
 .....X....................X.....
 XXXX.X.........X..........X.XXXX
 X.X..X....................X..X.X
 XXXX.X.........X..........X.XXXX
 .....X....................X.....
 X..X.X.........X..........X.XXXX
 X..X.X....................X..X.X
 XXXX.X.........X..........X.X.XX
end


 player0:
 %10111101
 %11111111
 %10111101
 %00111100
 %00111100
 %10111101
 %11111111
 %10011001
end

 player1:
 %10011001
 %11110111
 %10101101
 %00110100
 %00101100
 %10110101
 %11101111
 %10110101
end

 



mainloop

 rem Joystick controls
 if joy0left then player0x = player0x - 2
 if joy0right then player0x = player0x + 2
 if joy0up then player0y = player0y - 2
 if joy0down then mph = mph - 1


 rem if player hits wall, then slam them back some and away from wall.
 if player0x>=113 then player0y = player0y + 2:player0x = 111:mph=mph-5
 if player0x<=41 then player0y = player0y + 2:player0x = 43:mph=mph-5


 rem if the car hits the boundary, then start speeding up
 if player0y<70 then player0y = 70: mph = mph + 1

 rem set up vertical boundary to prevent driver from going off screen.
 if player0y>87 then player0y = 87

 if mph < 1 then mph = 1
 if mph > 100 then mph = 100


 badguymph = mph/20

 if badguymph<1 then badguymph = 1
 player1y = player1y + badguymph
 

 if player1y > 110 then player1y = 0:player1x = (rand&63)+46

 score = score + badguymph

 if collision(player0,player1) then goto loseloop



renderloop

 if !switchleftb then NUSIZ1 = $06

 drawscreen
 goto mainloop



loseloop
 drawscreen
 AUDV0 = 10
 AUDC0 = 8
 AUDF0 = 10

 if joy0fire then goto gameinit

 playfield:
 ................................
 ......XX................XX......
 ......XX................XX......
 ................................
 ................................
 ................................
 ....XXXXXXXXXXXXXXXXXXXXXXXX....
 ...XXX....................XXX...
 ...XX......................XX...
 ...XX......................XX...
 ................................
end

 goto loseloop

