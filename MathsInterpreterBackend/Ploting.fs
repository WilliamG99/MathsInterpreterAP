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

    /// Parses an equation and determines whether it is linear, quadratic, constant, or a simple assignment (x = value)
    let parseEquation (equation: string) : Equation =
        let sanitizedEquation = 
            equation.Replace(" ", "").Replace("²", "^2").Replace("^", "^").ToLower() // Sanitize input

        let quadraticRegex = @"y\s*=\s*(?<a>[-+]?\d*\.?\d+)?x(\^2)(\s*(?<b>[-+]\s*\d*\.?\d+)?x)?(\s*(?<c>[-+]\s*\d*\.?\d+)?)?$"
        let linearRegex = @"y\s*=\s*(?<b>[-+]?\d*\.?\d+|[-+]?)x\s*(?<c>[-+]\s*\d*\.?\d+)?$"
        let constantRegex = @"y\s*=\s*(?<c>[-+]?\d*\.?\d+)$"
    
        // Regex to match equations like x = 5 or y = 5
        let simpleEquationRegex = @"(?<variable>x|y)\s*=\s*(?<value>[-+]?\d*\.?\d+)$"

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
                    // Handle x = 5 or y = 5 as simple constant equations
                    let simpleMatch = Regex.Match(sanitizedEquation, simpleEquationRegex)
                    if simpleMatch.Success then
                        let variable = simpleMatch.Groups["variable"].Value
                        let value = tryParseFloat simpleMatch.Groups["value"] 0.0
                        // Return a constant equation based on the variable
                        if variable = "x" then
                            Constant(value)
                        else
                            Constant(value)
                    else
                        raise (System.ArgumentException "Invalid equation format. Use 'y = ax + b' or 'y = ax² + bx + c' or 'x = value'.")



    /// Creates a new PlotModel with default axes
    let createPlotModel (title: string) : PlotModel =
        let plotModel = PlotModel(Title = title)

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

    /// Linear interpolation between two points
    let linearInterpolation (x: float) (Dx: float) (f: float -> float) : float =
        // Linear interpolation between points (x-Dx) and (x+Dx)
        let x0 = x - Dx
        let x1 = x + Dx
        let y0 = f(x0)  // Assuming f(x) is the function we are plotting
        let y1 = f(x1)
        // Linear interpolation formula
        y0 + (x - x0) * (y1 - y0) / (x1 - x0)

    /// Create a range for floating-point numbers (since F# doesn't support float ranges directly)
    let createRange (start: float) (step: float) (stop: float) =
        seq {
            let mutable x = start
            while x <= stop do
                yield x
                x <- x + step
        }

    /// Plotting function with interpolation using for-loop
    let plotWithInterpolation (x_min: float) (x_max: float) (Dx: float) (I: float -> float -> (float -> float) -> float) (f: float -> float) (plotModel: PlotModel) =
        let lineSeries = LineSeries()
        for x in createRange x_min Dx x_max do
            // Apply interpolation
            let interpolatedValue = I x Dx f

            // Create the DataPoint separately
            let dataPoint = DataPoint(x, interpolatedValue)

            // Add the DataPoint to the series
            lineSeries.Points.Add(dataPoint)

        plotModel.Series.Clear()
        plotModel.Series.Add(lineSeries)

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

    /// Plots an equation based on its type with interpolation
    let plotEquation (equation: Equation) (x_min: float) (x_max: float) (Dx: float) (plotModel: PlotModel) =
        let interpolationFunc = linearInterpolation  // For now, we use linear interpolation
        let f (x: float) : float = 
            match equation with
            | Linear(slope, intercept) -> slope * x + intercept
            | Quadratic(a, b, c) -> a * x * x + b * x + c
            | Constant(value) -> value
        plotWithInterpolation x_min x_max Dx interpolationFunc f plotModel
