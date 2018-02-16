// Produce a gradient image that spans all colors of the rainbow.

package main

import (
	"fmt"
	"image"
	"image/color"
	"image/png"
	"math"
	"os"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

// Hsv creates a new Color given a Hue in [0..360], a Saturation and a Value in [0..1]
// Adapted from https://github.com/lucasb-eyer/go-colorful/blob/master/colors.go
// which is written by @lucasb-eyer based on http://en.wikipedia.org/wiki/HSL_and_HSV
func Hsv(H, S, V float64) color.RGBA {
	Hp := H / 60.0
	C := V * S
	X := C * (1.0 - math.Abs(math.Mod(Hp, 2.0)-1.0))

	m := V - C
	r, g, b := 0.0, 0.0, 0.0

	switch {
	case 0.0 <= Hp && Hp < 1.0:
		r = C
		g = X
	case 1.0 <= Hp && Hp < 2.0:
		r = X
		g = C
	case 2.0 <= Hp && Hp < 3.0:
		g = C
		b = X
	case 3.0 <= Hp && Hp < 4.0:
		g = X
		b = C
	case 4.0 <= Hp && Hp < 5.0:
		r = X
		b = C
	case 5.0 <= Hp && Hp < 6.0:
		r = C
		b = X
	}

	return color.RGBA{uint8(255 * (m + r)), uint8(255 * (m + g)), uint8(255 * (m + b)), 255}
}

func main() {
	const offset = 110
	const width = 500
	const height = 1
	img := image.NewRGBA(image.Rectangle{image.Point{0, 0},
		image.Point{width, height}})
	for x := 0; x < width; x++ {
		pix := Hsv(float64((offset+x)%width)/float64(width)*float64(360.0), 0.9, 0.9)
		//pix = Hsv(100.0, 0.9, 0.9)
		fmt.Println(pix)
		for y := 0; y < height; y++ {
			img.SetRGBA(x, y, pix)
		}
	}
	file, err := os.Create("rainbow.png")
	check(err)
	defer file.Close()
	png.Encode(file, img)
}
