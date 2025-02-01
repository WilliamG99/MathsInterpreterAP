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

        // Evaluate the expression for a range of x values
        for x in createRange x_min step x_max do
            let y = evaluate x expression  // Evaluate the expression for the current x
            lineSeries.Points.Add(DataPoint(x, y))

        // Clear existing series and add the new one
        plotModel.Series.Clear()
        plotModel.Series.Add(lineSeries)