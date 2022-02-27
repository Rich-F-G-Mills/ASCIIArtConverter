
open System
open System.Runtime.InteropServices;
open System.IO
open SkiaSharp


let [<Literal>] CharBlockWidth = 22
let [<Literal>] CharBlockHeight = 30
let [<Literal>] CharBlockInnerMargin = 0


let chars =
    { 32 .. 126 }
    |> Seq.map (Convert.ToChar >> string)


let fontPaint =
    new SKPaint(
        Color = SKColors.White,
        IsAntialias = false,
        TextSize = 40f,
        Typeface =
            SKTypeface.FromFamilyName("Courier New", SKFontStyle.Bold)
    )

let charSurfaceProps =
    new SKImageInfo(
        AlphaType = SKAlphaType.Opaque,
        ColorType = SKColorType.Gray8,
        Width = int CharBlockWidth,
        Height = int CharBlockHeight
    )

let charPaths =
    chars
    |> Seq.map (fun chr ->
        (chr, fontPaint.GetTextPath(chr, 0.0f, 0.0f)))
    |> Map.ofSeq

let charXScaling =
    let maxWidth =
        charPaths
        |> Map.values
        |> Seq.map (fun p -> p.TightBounds.Width)
        |> Seq.max

    ((float32 CharBlockWidth) - 2.0f * (float32 CharBlockInnerMargin)) / maxWidth

let charYScaling =
    let maxHeight =
        charPaths
        |> Map.values
        |> Seq.map (fun p -> p.TightBounds.Height)
        |> Seq.max

    ((float32 CharBlockHeight) - 2.0f * (float32 CharBlockInnerMargin)) / maxHeight

let charBaseline =
    let lowestPoint =
        charPaths
        |> Map.values
        |> Seq.map (fun p -> p.TightBounds.Bottom)
        |> Seq.max

    let heighestPoint =
        charPaths
        |> Map.values
        |> Seq.map (fun p -> p.TightBounds.Top)
        |> Seq.min

    let blockInnerHeight =
        (float32 CharBlockHeight) - 2.0f * (float32 CharBlockInnerMargin)

    (float32 (CharBlockHeight - CharBlockInnerMargin)) - blockInnerHeight * lowestPoint / (lowestPoint - heighestPoint)

let scaledCharPaths =
    let transformation =
        SKMatrix.CreateScaleTranslation(
            charXScaling,
            charYScaling,
            float32 CharBlockInnerMargin,
            charBaseline
        )

    charPaths
    |> Map.map (fun _ path ->
        let transformedPath =
            new SKPath(path)

        transformedPath.Transform(transformation)

        transformedPath)

let nativeRasterData =
    Marshal.AllocHGlobal(charSurfaceProps.BytesSize)

let charPixelIntensities =
    scaledCharPaths
    |> Map.map (fun chr path ->
        use surface =
            SKSurface.Create(charSurfaceProps)

        surface.Canvas.Clear(SKColors.Black)
        surface.Canvas.DrawPath(path, fontPaint)

        use image =
            surface.Snapshot()

        use imagePixels =
            new SKPixmap(charSurfaceProps, nativeRasterData)

        ignore <| image.ReadPixels(imagePixels)

        let rasterData =
            Array.zeroCreate<Byte> charSurfaceProps.BytesSize

        do Marshal.Copy(nativeRasterData, rasterData, 0, charSurfaceProps.BytesSize)

        rasterData
        |> Array.map (fun pixel -> (float32 pixel) / 255.0f))

do Marshal.FreeHGlobal(nativeRasterData)


let sourcePixels, sourceWidth, sourceHeight =
    use sourceImage =
        SKBitmap.Decode("TO_CONVERT.JPG")

    sourceImage.Pixels, sourceImage.Width, sourceImage.Height

let numHorizBlocks =
    sourceWidth / CharBlockWidth

let numVertBlocks =
    sourceHeight / CharBlockHeight

let sourceBlocks =
    Array2D.init numVertBlocks numHorizBlocks (fun rowIdx colIdx ->
    //Array2D.init 10 10 (fun rowIdx colIdx ->
        let startingIdx =
                colIdx * CharBlockWidth + rowIdx * CharBlockHeight * sourceWidth

        Array.init (CharBlockWidth * CharBlockHeight) (fun cellIdx ->
            let sourceIdx =
                startingIdx + (cellIdx % CharBlockWidth) + (cellIdx / CharBlockWidth * sourceWidth)

            sourcePixels[sourceIdx]))

let sourceBlockAvgColour =
    sourceBlocks
    |> Array2D.map (fun block ->
        let avgRed =
            block |> Array.averageBy (fun pixel -> float32 pixel.Red) |> byte

        let avgGreen =
            block |> Array.averageBy (fun pixel -> float32 pixel.Green) |> byte

        let avgBlue =
            block |> Array.averageBy (fun pixel -> float32 pixel.Blue) |> byte

        new SKColor(avgRed, avgGreen, avgBlue))

let sourceBlockPixelIntensities =
    sourceBlocks
    |> Array2D.map (fun block ->
        block |> Array.map (fun pixel ->
            let r, g, b =
                float32 pixel.Red, float32 pixel.Green, float32 pixel.Blue

            MathF.Sqrt((0.299f*r*r + 0.587f*g*g + 0.114f*b*b) / 255f / 255f) * 1.25f))

let fittedLetters =
    sourceBlockPixelIntensities
    |> Array2D.mapi (fun _ colIdx blockIntensities ->
        printf "."

        if colIdx = numHorizBlocks - 1 then
            printf "\n"

        charPixelIntensities
        |> Map.toSeq
        |> Seq.map (fun (chr, charIntensities) ->
            let totalDifference =
                blockIntensities
                |> Seq.zip charIntensities
                |> Seq.sumBy (fun (bI, cI) -> (bI - cI) * (bI - cI))

            (chr, totalDifference))
        |> Seq.minBy snd
        |> fst)

let outputSurfaceProps =
    new SKImageInfo(
        AlphaType = SKAlphaType.Opaque,
        ColorType = SKImageInfo.PlatformColorType,
        Width = sourceWidth,
        Height = sourceHeight
    )

let outputSurface =
    SKSurface.Create(outputSurfaceProps)

outputSurface.Canvas.Clear(SKColors.Black)

fittedLetters
|> Array2D.iteri (fun rowIdx colIdx chr ->
    outputSurface.Canvas.ResetMatrix()

    outputSurface.Canvas.Translate(
        float32 <| colIdx * CharBlockWidth,
        float32 <| (rowIdx + 1) * CharBlockHeight
    )

    outputSurface.Canvas.DrawPath(
        scaledCharPaths[chr],
        new SKPaint(
            Color = sourceBlockAvgColour[rowIdx, colIdx],
            IsAntialias = true
        ))
    )

let outputStream =
    new FileStream("RENDERED.PNG", FileMode.Create, FileAccess.Write)

let image =
    outputSurface.Snapshot()

let imageEncoded =
    image.Encode()
    
do imageEncoded.SaveTo(outputStream)

do outputStream.Close()