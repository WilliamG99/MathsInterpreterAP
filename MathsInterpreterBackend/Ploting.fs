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
                    x_min, x_max+step

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

