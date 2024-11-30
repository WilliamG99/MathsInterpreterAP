namespace MathsInterpreterBackend

module Plotting =

    open OxyPlot
    open OxyPlot.Series
    open OxyPlot.Axes
    open System.Text.RegularExpressions

    // Define types to represent equations
    type Equation =
        | Linear of slope: float * intercept: float
        | Quadratic of a: float * b: float * c: float
        | Constant of value: float

    /// Helper function to extract matched float value from regex group
    let tryParseFloat (group: Group) defaultValue =
        if group.Success then 
            float (group.Value.Replace(" ", "")) 
        else 
            defaultValue

    /// Parses an equation and determines whether it is linear, quadratic, or constant.
    let parseEquation (equation: string) : Equation =
        let sanitizedEquation = equation.Replace(" ", "").Replace("²", "^2")
        let quadraticRegex = @"y\s*=\s*(?<a>[-+]?\d*\.?\d+)?x(\^2|²)(\s*(?<b>[-+]\s*\d*\.?\d+)?x)?(\s*(?<c>[-+]\s*\d*\.?\d+)?)?$"
        let linearRegex = @"y\s*=\s*(?<b>[-+]?\d*\.?\d+|[-+]?)x\s*(?<c>[-+]\s*\d*\.?\d+)?$"
        let constantRegex = @"y\s*=\s*(?<c>[-+]?\d*\.?\d+)$"

        let quadraticMatch = Regex.Match(sanitizedEquation, quadraticRegex)
        if quadraticMatch.Success then
            let a = tryParseFloat quadraticMatch.Groups.["a"] 1.0
            let b = tryParseFloat quadraticMatch.Groups.["b"] 0.0
            let c = tryParseFloat quadraticMatch.Groups.["c"] 0.0
            Quadratic(a, b, c)
        else
            let linearMatch = Regex.Match(sanitizedEquation, linearRegex)
            if linearMatch.Success then
                let slope = tryParseFloat linearMatch.Groups.["b"] 1.0
                let intercept = tryParseFloat linearMatch.Groups.["c"] 0.0
                Linear(slope, intercept)
            else
                let constantMatch = Regex.Match(sanitizedEquation, constantRegex)
                if constantMatch.Success then
                    let value = float constantMatch.Groups.["c"].Value
                    Constant(value)
                else
                    raise (System.ArgumentException "Invalid equation format. Use 'y = ax + b' or 'y = ax² + bx + c'.")

    /// Creates a new PlotModel with default axes
    let createPlotModel (title: string) : PlotModel =
        let plotModel = PlotModel(Title = title)

        let axisStrokeThickness = 3.0  // Adjust the value to make the line thicker


        plotModel.Axes.Add(
            LinearAxis(
                Position = AxisPosition.Bottom,
                Title = "X",
                Minimum = -10.0,
                Maximum = 10.0,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot

            )
        )
        plotModel.Axes.Add(
            LinearAxis(
                Position = AxisPosition.Left,
                Title = "Y",
                Minimum = -10.0,
                Maximum = 10.0,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot

            )
        )
        plotModel

    /// Plots a linear equation on the given PlotModel
    let plotLine slope intercept (plotModel: PlotModel) =
        let lineSeries = LineSeries(Title = sprintf "y = %.2fx + %.2f" slope intercept)
        for x in -10.0 .. 0.1 .. 10.0 do
            let y = slope * x + intercept
            lineSeries.Points.Add(DataPoint(x, y))
        plotModel.Series.Clear()
        plotModel.Series.Add(lineSeries)

    /// Plots a quadratic equation on the given PlotModel
    let plotCurve a b c (plotModel: PlotModel) =
        let curveSeries = LineSeries(Title = sprintf "y = %.2fx² + %.2fx + %.2f" a b c)
        for x in -10.0 .. 0.1 .. 10.0 do
            let y = a * x * x + b * x + c
            curveSeries.Points.Add(DataPoint(x, y))
        plotModel.Series.Clear()
        plotModel.Series.Add(curveSeries)

    /// Plots a constant equation on the given PlotModel
    let plotConstant value (plotModel: PlotModel) =
        let constantSeries = LineSeries(Title = sprintf "y = %.2f" value)
        for x in -10.0 .. 0.1 .. 10.0 do
            constantSeries.Points.Add(DataPoint(x, value))
        plotModel.Series.Clear()
        plotModel.Series.Add(constantSeries)

    /// Plots an equation based on its type
    let plotEquation (equation: Equation) (plotModel: PlotModel) =
        match equation with
        | Linear(slope, intercept) -> plotLine slope intercept plotModel
        | Quadratic(a, b, c) -> plotCurve a b c plotModel
        | Constant(value) -> plotConstant value plotModel
