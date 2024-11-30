using System;
using System.Windows;
using System.Windows.Controls;
using OxyPlot;
using OxyPlot.Series;
using OxyPlot.Axes;
using System.Text.RegularExpressions;

namespace MathsInterpreterGUI
{
    public partial class MainWindow : Window
    {
        // A plot model that will represent the graph in the UI
        private PlotModel plotModel;

        // Constructor: Initializes the components of the window and prepares the plot.
        public MainWindow()
        {
            InitializeComponent();
            InitializePlot();  // Call to initialize the plot area when the window opens
        }
        private void ShowSymbolsButton_Click(object sender, RoutedEventArgs e)
        {
            SymbolsPanel.Visibility = Visibility.Visible;
            TrigPanel.Visibility = Visibility.Collapsed;
        }

        private void ShowTrigButton_Click(object sender, RoutedEventArgs e)
        {
            SymbolsPanel.Visibility = Visibility.Collapsed;
            TrigPanel.Visibility = Visibility.Visible;
        }


        // Event handler for inserting symbols (numbers or letters) into the input field when a button is clicked
        private void InsertSymbol_Click(object sender, RoutedEventArgs e)
        {
            // Get the button that was clicked
            var button = sender as Button;

            // Ensure the button is not null before trying to access its content
            if (button != null)
            {
                // Add the button's content (symbol or number) to the ExpressionTextBox
                ExpressionTextBox.Text += button.Content.ToString();
            }
        }

        // Clears the expression input, result, and any error messages displayed
        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            ExpressionTextBox.Clear();  // Clear the input field
            ResultTextBlock.Text = string.Empty;  // Clear the result text block
            ErrorTextBlock.Text = string.Empty;   // Clear the error text block
        }

        // Appends the symbol or number from the clicked button to the input field
        private void Button_Click(object sender, RoutedEventArgs e)
        {
            string buttonText = (sender as Button).Content.ToString();
            ExpressionTextBox.Text += buttonText;  // Add clicked button content to the text box
        }

        // Evaluates the mathematical expression using a backend parser and displays the result
        private void CalculateButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                // Call backend to evaluate the expression entered in the input field
                var result = MathsInterpreterBackend.Parser.interpret(ExpressionTextBox.Text);

                // Display the result of the calculation
                ResultTextBlock.Text = "= " + result.ToString();
                ErrorTextBlock.Text = string.Empty; // Clear any previous error message

                // Update the list of variables after the calculation
                UpdateVariableList();
            }
            catch (Exception ex)
            {
                // If there's an error, display the error message
                ErrorTextBlock.Text = $"Error: {ex.Message}";
                ResultTextBlock.Text = string.Empty; // Clear the result field
            }
        }

        // Updates the variable list displayed in the "Table" tab after evaluation
        private void UpdateVariableList()
        {
            try
            {
                var variables = MathsInterpreterBackend.Parser.getSymbolList();

                // Clear existing items in the table
                Table.Items.Clear();
                foreach (var variable in variables)
                {
                    // Display variable name, value, and type in the table
                    string displayText = $"{variable.Key}: {variable.Value} ({variable.Type})";
                    Table.Items.Add(displayText);
                }
            }
            catch (Exception ex)
            {
                // If there's an error while updating the variable list, show an error message
                MessageBox.Show($"Error updating variable list: {ex.Message}", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        // Initializes the plotting area and sets the default settings for the plot
        private void InitializePlot()
        {
            System.Diagnostics.Debug.WriteLine("Initializing PlotModel...");
            plotModel = new PlotModel { Title = "Graph" };

            // Set up X-axis with range from -10 to 10
            plotModel.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Bottom,
                Title = "X",
                Minimum = -10,
                Maximum = 10,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot
            });

            // Set up Y-axis with range from -10 to 10
            plotModel.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Left,
                Title = "Y",
                Minimum = -10,
                Maximum = 10,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot
            });

            // Link the plot model to the plot view component in the UI
            PlotView.Model = plotModel;
            System.Diagnostics.Debug.WriteLine("PlotModel initialized.");
        }

        // Parses the entered equation and chooses whether to plot a quadratic or linear equation
        private void OnPlotLineClick(object sender, RoutedEventArgs e)
        {
            try
            {
                string equation = GraphEquationInput.Text; // Get the equation input from the user
                System.Diagnostics.Debug.WriteLine($"Button clicked. Parsing equation: {equation}");

                var (isQuadratic, a, b, c) = ParseEquation(equation);

                // Depending on whether it's quadratic or linear, call the respective plot function
                if (isQuadratic)
                {
                    System.Diagnostics.Debug.WriteLine($"Detected quadratic equation. Coefficients: a={a}, b={b}, c={c}");
                    PlotCurve(a, b, c);
                }
                else
                {
                    System.Diagnostics.Debug.WriteLine($"Detected linear equation. Coefficients: slope={b}, intercept={c}");
                    PlotLine(b, c);
                }
            }
            catch (Exception ex)
            {
                // If the equation is invalid, show an error message
                System.Diagnostics.Debug.WriteLine($"Error in OnPlotLineClick: {ex.Message}");
                MessageBox.Show($"Invalid input: {ex.Message}", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        // Plots a linear equation based on slope and intercept
        private void PlotLine(double slope, double intercept)
        {
            System.Diagnostics.Debug.WriteLine($"Plotting line with slope={slope}, intercept={intercept}");
            plotModel.Series.Clear();  // Clear previous series

            // Create a new line series
            var lineSeries = new LineSeries
            {
                Title = $"y = {slope}x {((intercept >= 0) ? $"+ {intercept}" : $"{intercept}")}",
                StrokeThickness = 2
            };

            // Add points for the line from x = -10 to x = 10
            for (double x = -10; x <= 10; x += 0.1)
            {
                double y = slope * x + intercept;
                lineSeries.Points.Add(new DataPoint(x, y));
            }

            // Add the line series to the plot model and update the plot
            plotModel.Series.Add(lineSeries);
            plotModel.InvalidatePlot(true);
            System.Diagnostics.Debug.WriteLine("Line plot completed.");
        }

        // Plots a quadratic equation based on its coefficients
        private void PlotCurve(double a, double b, double c)
        {
            System.Diagnostics.Debug.WriteLine($"Plotting curve with coefficients: a={a}, b={b}, c={c}");
            plotModel.Series.Clear();  // Clear previous series

            // Create a new curve series
            var curveSeries = new LineSeries
            {
                Title = $"y = {a}x² {((b >= 0) ? $"+ {b}" : $"{b}")}x {((c >= 0) ? $"+ {c}" : $"{c}")}",
                StrokeThickness = 2
            };

            // Add points for the curve from x = -10 to x = 10
            for (double x = -10; x <= 10; x += 0.1)
            {
                double y = a * x * x + b * x + c;
                curveSeries.Points.Add(new DataPoint(x, y));
            }

            // Add the curve series to the plot model and update the plot
            plotModel.Series.Add(curveSeries);
            plotModel.InvalidatePlot(true);
            System.Diagnostics.Debug.WriteLine("Curve plot completed.");
        }

        // Parses an equation and determines whether it is linear or quadratic.
        private (bool isQuadratic, double a, double b, double c) ParseEquation(string equation)
        {
            System.Diagnostics.Debug.WriteLine($"Parsing Equation: {equation}");

            // Sanitize input to normalize formatting (optional, if inconsistent user inputs are a problem)
            equation = equation.Replace(" ", ""); // Remove all spaces
            equation = equation.Replace("²", "^2"); // Normalize "²" to "^2"

            // Regex for quadratic equations
            var quadraticRegex = new Regex(@"y\s*=\s*(?<a>[-+]?\d*\.?\d+)?x(\^2|²)(\s*(?<b>[-+]\s*\d*\.?\d+)?x)?(\s*(?<c>[-+]\s*\d*\.?\d+)?)?$");

            // Regex for linear equations
            var linearRegex = new Regex(@"y\s*=\s*(?<b>[-+]?\d*\.?\d+|[-+]?)x\s*(?<c>[-+]\s*\d*\.?\d+)?$");

            // Regex for constant equations (y = c)
            var constantRegex = new Regex(@"y\s*=\s*(?<c>[-+]?\d*\.?\d+)$");

            // Match quadratic equations
            if (quadraticRegex.IsMatch(equation))
            {
                System.Diagnostics.Debug.WriteLine("Quadratic equation matched.");
                var match = quadraticRegex.Match(equation);

                double a = match.Groups["a"].Success ? double.Parse(match.Groups["a"].Value) : 1;
                double b = match.Groups["b"].Success ? double.Parse(match.Groups["b"].Value.Replace(" ", "")) : 0;
                double c = match.Groups["c"].Success ? double.Parse(match.Groups["c"].Value.Replace(" ", "")) : 0;

                System.Diagnostics.Debug.WriteLine($"Parsed coefficients: a={a}, b={b}, c={c}");
                return (true, a, b, c);
            }
            else if (linearRegex.IsMatch(equation))
            {
                System.Diagnostics.Debug.WriteLine("Linear equation matched.");
                var match = linearRegex.Match(equation);

                double slope = match.Groups["b"].Success ? (match.Groups["b"].Value == "-" ? -1 : double.Parse(match.Groups["b"].Value)) : 1;
                double intercept = match.Groups["c"].Success ? double.Parse(match.Groups["c"].Value.Replace(" ", "")) : 0;

                System.Diagnostics.Debug.WriteLine($"Parsed coefficients: slope={slope}, intercept={intercept}");
                return (false, 0, slope, intercept);
            }
            else if (constantRegex.IsMatch(equation))
            {
                System.Diagnostics.Debug.WriteLine("Constant equation matched.");
                var match = constantRegex.Match(equation);

                double c = double.Parse(match.Groups["c"].Value);

                System.Diagnostics.Debug.WriteLine($"Parsed constant: c={c}");
                return (false, 0, 0, c); // No x term means slope and quadratic coefficient are 0
            }
            else
            {
                System.Diagnostics.Debug.WriteLine("Equation did not match any pattern.");
                throw new ArgumentException("Invalid equation format. Use the form 'y = ax + b' or 'y = ax² + bx + c'.");
            }
        }
    }
}
