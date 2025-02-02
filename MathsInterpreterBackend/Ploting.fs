namespace MathsInterpreterBackend

module Plotting =

    open OxyPlot
    open OxyPlot.Series
    open OxyPlot.Axes
    open Lexer  // Import the Lexer module
    open Parser // Import the Parser module
    open Interpreter

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

    /// Create a range for floating-point numbers (since F# doesn't support float ranges directly)
    let createRange (start: float) (step: float) (stop: float) =
        seq {
            let mutable x = start
            while x <= stop do
                yield x
                x <- x + step
        }

    /// Evaluates a mathematical expression for a given x value
    let evaluate (x: float) (expression: string) : float =
        // Replace "x" in the expression with the current x value
        let input = expression.Replace("x", sprintf "(%f)" x)  // Ensure negative values are properly parenthesized

        // Tokenize and parse the input
        let tokens = Lexer.lexer input
        let _, result = Parser.evaluateExpression tokens

        // Convert the result to a float
        match result with
        | Int i -> float i
        | Float f -> f
        | _ -> raise (System.Exception("Invalid expression for plotting"))

    /// Plots a generic expression on the given PlotModel
    let plotExpression (expression: string) (x_min: float) (x_max: float) (step: float) (plotModel: PlotModel) =
        let lineSeries = LineSeries(Title = expression)

        // Set a default step value if step is NaN
        let step = if System.Double.IsNaN(step) then 0.1 else step

        // Function to update the plot based on the current axis range
        let updatePlot () =
            let xAxis = plotModel.Axes.[0] :?> LinearAxis

            // Determine the range to use
            let rangeMin, rangeMax =
                // If x_min or x_max is NaN, use the dynamic range
                if System.Double.IsNaN(x_min) || System.Double.IsNaN(x_max) then
                    xAxis.ActualMinimum, xAxis.ActualMaximum
                else
                    // Otherwise, use the user-defined range
                    x_min, x_max

            // Clear the existing points
            lineSeries.Points.Clear()

            // Re-evaluate the expression for the determined range
            for x in createRange rangeMin step rangeMax do
                let y = evaluate x expression
                lineSeries.Points.Add(DataPoint(x, y))

            // Refresh the plot
            plotModel.InvalidatePlot(true)

        // Initial plot with the determined range
        updatePlot()

        // Subscribe to the AxisChanged event for the X axis
        let xAxis = plotModel.Axes.[0] :?> LinearAxis
        xAxis.AxisChanged.Add(fun _ -> updatePlot())

        // Clear existing series and add the new one
        plotModel.Series.Clear()
        plotModel.Series.Add(lineSeries)

    let cleanUpExpression (expr: string) : string =
        expr
        |> fun s -> s.Replace("+ -", "-")  // Change "+ -" to just "-"
        |> fun s -> s.Replace(" + 0", "").Replace("0 + ", "") // Remove "+ 0" cases
        |> fun s -> if s.StartsWith("+ ") then s.Substring(2) else s // Remove leading "+ " if present

    let deriveExpression (expression: string) : string =
        // Tokenize the input expression
        let tokens = lexer expression

        // Differentiate the tokens and return the result as a string
        let (derivative, _) = differentiate tokens

        // Clean up unnecessary symbols
        (cleanUpExpression derivative)


    /// Plots the tangent line to a function at a specific point
    let plotTangentLine (expression: string) (x_point: float) (plotModel: PlotModel) =
        // Evaluate the function at the given point to get the y-coordinate
        let y_point = evaluate x_point expression

        // Compute the derivative (slope) at the given point
        // Tokenize the input expression
        let tokens = lexer expression
        // Differentiate the tokens and return the result as a string
        let (derivative, _) = differentiate tokens
        // Evaluate the derivative at the given point
        let slope = evaluate x_point derivative

        // Compute the y-intercept of the tangent line using the point-slope form: y = mx + b
        let y_intercept = y_point - slope * x_point

        // Create a LineSeries for the tangent line
        let tangentLineSeries = LineSeries(Title = sprintf "Tangent at x = %f" x_point)

        // Evaluate the tangent line for a range of x values
        let x_min = plotModel.Axes.[0].ActualMinimum
        let x_max = plotModel.Axes.[0].ActualMaximum
        for x in createRange x_min 0.1 x_max do
            let y = slope * x + y_intercept
            tangentLineSeries.Points.Add(DataPoint(x, y))

        // Add the tangent line to the plot
        plotModel.Series.Add(tangentLineSeries)

        //plotModel.Title <- sprintf "Tangent at x point: %f" x_point

        // Refresh the plot
        plotModel.InvalidatePlot(true)