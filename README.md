# rnn-bot
RNN (Recurrent Neural Network) based agent navigating space filled with asteroids. 
[Video here](https://twitter.com/indy9000/status/867274225995984896/video/1)

This is a working progress simulation game but for robots. The spaceship is controlled by an RNN which learns to get to the other side of the screen through an asteroid field. Learning is through an evolutionary strategy (GA). The spaceship has sensors (directional radar if you wish) to sense the world around it. Each sensor tells how far an object is. It has horizontal and vertical thrusters. Physics used is elementary Newtonian:  More force, more accelleration. Fuel is limited and accelleration uses up the fuel. If spaceship hits an asteroid, that's the end of it. Which way to look and move are decided by RNN.

# How to get it running
Code is Fsharp, which should be compiled using the [Fable compiler](https://fable.io) to Javascript. Then load the index1.html in the browser. No need for a webserver.

# Interesing bits
During the experimentation I've created various utils 
* RNN implementation
* FNN - Feed forward implementation 
* GA - Evolutionary strategy
* Game Logic and Graphics - collision detection, line intersection, drawing this and that,  etc..
* Vector type
* Chart type
* Various random generators, shuffling indices, scaling (exponential, linear), rad/degree convertors, etc you may find useful.
