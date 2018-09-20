# uv-26-countermeasures-avr-clone
An assembly language programming project from FGCU Fall 2017. A simple clone of the functionality of the UV-26 countermeasures 
panel found in the Ka-50 attack helicopter.

## Original text of project proposal, for context

To defeat infrared-guided heat-seeking missiles, modern military aircraft dispense hot flares to confuse incoming IR missile seeker 
heads.

The Ka-50 Black Shark, a Russian attack helicopter, has such a system, called the UV-26 countermeasures system. The Ka-50 contains 64 
flares in each winglet-mounted dispenser, for a total of 128. The UV-26 system is operated via a small control panel in the top right of 
the cockpit, where the pilot can create and execute various simple dispensing programs to react to different threat environments. I  
intend to replicate the behavior of this panel.

The system is completely isolated from every other system of the helicopter for redundancy, so the scope of the project is relatively 
contained.

A red, 3-digit 7-segment display displays the current flare dispensing program. The two yellow LEDs on the board will be used to model 
the dispensing of flares, flashing for 1/10 second to represent a flare being fired, and the LCD screen on the screen will replace the 
7-segment display.

In the real word, a three-position switch is used to select which winglet will dispense flares – Left, Right or Both. This is modeled by 
a button in my version.

Countermeasures dispensing programs are built as follows:

Program Digit - Meaning

    1 - Number of sequences of dispensing to run.
    
        Special case:
    
        0 = Infinite sequences
    
    2 - Number of flares to dispense per sequence

    3 - Delay between sequences, in seconds.

        Special cases:
    
        7 = 0.25 s
    
        9 = 0.5 s
    
        0 = 0.125 s
    

The dispensing interval of flares within a sequence is a hardware limitation and is assumed to be 0.1 seconds in my implementation.

External buttons have been ordered for use with this project. The intended functions of all buttons are given below:

    Button - Function
        1 - Select LEFT/RIGHT/BOTH winglets.
        2 - Start dispensing
        3 - Stop dispensing
        External 1 - Increment program digit 1 (Sequences)
        External 2 - Increment program digit 2 (Quantity)
        External 3 - Increment program digit 3 (Interval)
        External 4 - Toggle programming mode

Note that there is an additional button on the real system that is not modeled, which resets to the default program. This can be 
simulated by resetting the board itself.

Additionally, the program will use several LEDs to indicate state, show below:

    LED	- Meaning
    Top Yellow	- Simulate dispensing left flare
    Bottom Yellow - Simulate dispensing right flare
    Red	- Left winglet selected
    Green - Right winglet selected
    Orange - (Red and Green together) Both winglets selected

Note that the choices of the red and green LEDs are perfect, since a red light represents the left side of a boat or plane, and green 
represents the left.

Some example dispensing programs, and their intended execution are:
110 – Fires 1 sequence of 1 flare.
221 – Fires 2 sequences of 2 flares with 1 second between sequences.
037 – Fires infinite sequences of 3 flares with 0.5 seconds between sequences.
123 – Fires 1 sequence of 2 flares with three seconds between sequences.

The original goal was to use an actual 7-segment display rather than the LCD, but shipping delays make this impossible.
