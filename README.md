# Image Compressor
[K-mean algorithm](https://en.wikipedia.org/wiki/K-means_clustering) applied to image compression.
<br>

### Makefile
```sh
$> git clone [https://github.com/Harmos274/ImageCompressor](https://github.com/MartinRamdane/Image-compressor.git)
$> cd Image-compressor
$> make
```
<br>

## How to use it ?
```sh
$> ./imageCompressor -h
USAGE: ./imageCompressor n e input

	n		number of colors in the final image
	e		convergence limit
	input	path of the file containing the colors of the pixels
```
<br>

## Input Format
```sh
$> head exampleInput
(0,0) (33,18,109)
(0,1) (33,18,109)
(0,2) (33,21,109)
(0,3) (33,21,112)
(0,4) (33,25,112)
(0,5) (33,32,112)
(1,0) (33,18,109)
(1,1) (35,18,109)
(1,2) (35,21,109)
(1,3) (38,21,112)
```
The left column is the actual position of a pixel and the right column it's color.
<br>
