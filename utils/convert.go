package main

import (
    "image"
    "image/color"
    "image/png"
    "os"
)

func main() {
    fileData, err := os.ReadFile("../burn.dat")
    if err != nil {
        panic(err)
    }

    const width, height = 320, 166
    img := image.NewPaletted(image.Rect(0, 0, width, height), nil)

    // Decode the RLE data from the start of the file
    var dataIndex, x, y int
    for dataIndex < len(fileData)-768 { // Assuming the last 768 bytes are the palette
        byteValue := fileData[dataIndex]
        dataIndex++

        if byteValue >= 192 { // RLE encoded
            count := byteValue - 192
            colorIndex := fileData[dataIndex]
            dataIndex++
            for i := 0; i < int(count); i++ {
                img.SetColorIndex(x, y, colorIndex)
                x++
                if x >= width {
                    x = 0
                    y++
                    if y >= height {
                        break
                    }
                }
            }
        } else { // Not RLE encoded
            img.SetColorIndex(x, y, byteValue)
            x++
            if x >= width {
                x = 0
                y++
                if y >= height {
                    break
                }
            }
        }
    }

    // Parse the palette
    paletteStart := len(fileData) - 768
    for i := 0; i < 256; i++ {
        r := fileData[paletteStart+i*3]
        g := fileData[paletteStart+i*3+1]
        b := fileData[paletteStart+i*3+2]
        img.Palette = append(img.Palette, color.RGBA{r, g, b, 255})
    }

    // Save the image as PNG
    outputFile, err := os.Create("../img/burn.png")
    if err != nil {
        panic(err)
    }
    defer outputFile.Close()

    if err := png.Encode(outputFile, img); err != nil {
        panic(err)
    }
}
