Networks-fun!
By: (Define TEAM "Awesome")
*Please run Networks-and-Run.rkt from terminal*

File Decriptions: 
-Networks-and-Run.rkt: 
Houses all the function involving signal-play and networks. It also makes the call to big-bang to begin the program. Requires Structs-Help-Setup.rkt, Events-and-Draw.rkt

-Events-and-Draw.rkt: 
Contains all the funtions necessary for big-bang. Requires Structs-Help-Setup.rkt

-Structs-Help-Setup.rkt:
Defines all structures, sets up useful global constants, and defines helpful functions that involve the structs defined.

Notes: 
Speed slider: controls how fast the song is played along with the pitch as they are connected

Distortion Cut-off Slider: a cut-off anywhere between 0.0 to 1.0 of each signal so that if a signal's magnitude is greater than the cut off it is changed to the cut-off

Distortion Scale Slider: Instead of cutting off all signal magnitudes greater than the cut off the value above the cut-off is scaled from 0.0 to 1.0 

Example of distortion: if the cut-off is set to 0 the scale slider becomes basically a volume slider.

Delay Slider: controls a delayed version of the being played along side it. The delay ranges from 0 to 1/10th of a second behind the main playhead. 

Flange Frequency Slider: controls the frequency of the flange if turned on. It ranges from 0.1 to 5 cycles per second.

Flange Button: toggles if a flange is created or just a delayed version of the song is played.

R buttons: are reset buttons for the slider they are under.

sound button: provides a drop down menu to select any .wav file in the Sounds directory. (if a file that doesn't have the extension .wav is placed in the Sounds directory, the program will break)

draw button: opens a second window showing the visualization of the signals being created. Shows the prior 1/28th of a second worth of signals