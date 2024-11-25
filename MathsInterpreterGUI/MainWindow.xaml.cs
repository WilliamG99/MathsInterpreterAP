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
        private PlotModel plotModel;

        public MainWindow()
        {
            InitializeComponent();
            InitializePlot();
        }

        private bool IsLetterMode = false; // Tracks the toggle state (numeric or letter mode)

        private void ToggleModeButton_Click(object sender, RoutedEventArgs e)
        {
            // Toggle the mode state
            IsLetterMode = !IsLetterMode;

            // Update the button content and the buttons' text
            if (IsLetterMode)
            {
                ToggleModeButton.Content = "1↔x"; // Change button text to indicate mode
                UpdateButtonContentToLetters();
            }
            else
            {
                ToggleModeButton.Content = "x↔1"; // Change button text to indicate mode
                UpdateButtonContentToNumbers();
            }
        }

        private void UpdateButtonContentToLetters()
        {
            // Example: Map buttons to letters in "letter mode"
            Num7Button.Content = "x";
            Num8Button.Content = "y";
            Num9Button.Content = "z";
            Num4Button.Content = "a";
            Num5Button.Content = "b";
            Num6Button.Content = "c";
            Num1Button.Content = "m";
            Num2Button.Content = "n";
            Num3Button.Content = "p";
            Num0Button.Content = "q";
        }

        private void UpdateButtonContentToNumbers()
        {
            // Example: Map buttons back to numbers in "numeric mode"
            Num7Button.Content = "7";
            Num8Button.Content = "8";
            Num9Button.Content = "9";
            Num4Button.Content = "4";
            Num5Button.Content = "5";
            Num6Button.Content = "6";
            Num1Button.Content = "1";
            Num2Button.Content = "2";
            Num3Button.Content = "3";
            Num0Button.Content = "0";
        }


        private void InsertSymbol_Click(object sender, RoutedEventArgs e)
        {
            // Get the button that was clicked
            var button = sender as Button;

            // Ensure the button is not null
            if (button != null)
            {
                // Append the button's content to the ExpressionTextBox
                ExpressionTextBox.Text += button.Content.ToString();
            }
        }


        /// <summary>
        /// Clears the input field, result display, and error messages.
        /// </summary>
        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            ExpressionTextBox.Clear();
            ResultTextBlock.Text = string.Empty;
            ErrorTextBlock.Text = string.Empty;
        }

        /// <summary>
        /// Appends the symbol or number from the clicked button to the input field.
        /// </summary>
        private void Button_Click(object sender, RoutedEventArgs e)
        {
            string buttonText = (sender as Button).Content.ToString();
            ExpressionTextBox.Text += buttonText;
        }

        /// <summary>
        /// Evaluates the mathematical expression using the backend parser.
        /// </summary>
        private void CalculateButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                // Call backend to evaluate the expression
                var result = MathsInterpreterBackend.Parser.interpret(ExpressionTextBox.Text);

                // Display the result
                ResultTextBlock.Text = "= " + result.ToString();
                ErrorTextBlock.Text = string.Empty;

                // Update the variable list
                UpdateVariableList();
            }
            catch (Exception ex)
            {
                // Display error message if evaluation fails
                ErrorTextBlock.Text = $"Error: {ex.Message}";
                ResultTextBlock.Text = string.Empty;
            }
        }

        /// <summary>
        /// Updates the variable list in the "Table" tab.
        /// </summary>
        private void UpdateVariableList()
        {
            try
            {
                var variables = MathsInterpreterBackend.Parser.getSymbolList();

                Table.Items.Clear();
                foreach (var variable in variables)
                {
                    string displayText = $"{variable.Key}: {variable.Value} ({variable.Type})";
                    Table.Items.Add(displayText);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error updating variable list: {ex.Message}", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        /// <summary>
        /// Initializes the plotting area with default settings.
        /// </summary>
        private void InitializePlot()
        {
            System.Diagnostics.Debug.WriteLine("Initializing PlotModel...");
            plotModel = new PlotModel { Title = "Graph" };

            plotModel.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Bottom,
                Title = "X",
                Minimum = -10,
                Maximum = 10,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot
            });

            plotModel.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Left,
                Title = "Y",
                Minimum = -10,
                Maximum = 10,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot
            });

            PlotView.Model = plotModel;
            System.Diagnostics.Debug.WriteLine("PlotModel initialized.");
        }

        /// <summary>
        /// Parses and plots an equation entered in the plotting tab.
        /// </summary>
        private void OnPlotLineClick(object sender, RoutedEventArgs e)
        {
            try
            {
                string equation = GraphEquationInput.Text;
                System.Diagnostics.Debug.WriteLine($"Button clicked. Parsing equation: {equation}");

                var (isQuadratic, a, b, c) = ParseEquation(equation);

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
                System.Diagnostics.Debug.WriteLine($"Error in OnPlotLineClick: {ex.Message}");
                MessageBox.Show($"Invalid input: {ex.Message}", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }


        /// <summary>
        /// Plots a linear equation given the slope and intercept.
        /// </summary>
        private void PlotLine(double slope, double intercept)
        {
            System.Diagnostics.Debug.WriteLine($"Plotting line with slope={slope}, intercept={intercept}");
            //LoadSymbolTable();
            plotModel.Series.Clear();

            var lineSeries = new LineSeries
            {
                Title = $"y = {slope}x {((intercept >= 0) ? $"+ {intercept}" : $"{intercept}")}",
                StrokeThickness = 2
            };

            for (double x = -10; x <= 10; x += 0.1)
            {
                double y = slope * x + intercept;
                lineSeries.Points.Add(new DataPoint(x, y));
            }

            plotModel.Series.Add(lineSeries);
            plotModel.InvalidatePlot(true);
            System.Diagnostics.Debug.WriteLine("Line plot completed.");
        }

        /// <summary>
        /// Plots a quadratic equation given its coefficients.
        /// </summary>
        private void PlotCurve(double a, double b, double c)
        {
            System.Diagnostics.Debug.WriteLine($"Plotting curve with coefficients: a={a}, b={b}, c={c}");
            //LoadSymbolTable();
            plotModel.Series.Clear();

            var curveSeries = new LineSeries
            {
                Title = $"y = {a}x² {((b >= 0) ? $"+ {b}" : $"{b}")}x {((c >= 0) ? $"+ {c}" : $"{c}")}",
                StrokeThickness = 2
            };

            for (double x = -10; x <= 10; x += 0.1)
            {
                double y = a * x * x + b * x + c;
                curveSeries.Points.Add(new DataPoint(x, y));
            }

            plotModel.Series.Add(curveSeries);
            plotModel.InvalidatePlot(true);
            System.Diagnostics.Debug.WriteLine("Curve plot completed.");
        }

        /// <summary>
        /// Parses an equation and determines whether it is linear or quadratic.
        /// </summary>
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
