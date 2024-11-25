using System;
using System.Windows;
using System.Windows.Controls;
using OxyPlot;
using OxyPlot.Series;
using OxyPlot.Axes;

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
        }

        /// <summary>
        /// Parses and plots an equation entered in the plotting tab.
        /// </summary>
        private void OnPlotLineClick(object sender, RoutedEventArgs e)
        {
            try
            {
                string equation = GraphEquationInput.Text;


                // Parse equation and determine if linear or quadratic
                var (isLinear, slope, intercept, a, b, c) = ParseEquation(equation);

                if (isLinear)
                {
                    PlotLine(slope, intercept);
                }
                else
                {
                    PlotCurve(a, b, c);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Invalid equation: {ex.Message}", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        /// <summary>
        /// Plots a linear equation given the slope and intercept.
        /// </summary>
        private void PlotLine(double slope, double intercept)
        {
            plotModel.Series.Clear();

            var lineSeries = new LineSeries
            {
                Title = $"y = {slope}x + {intercept}",
                StrokeThickness = 2
            };

            for (double x = -10; x <= 10; x += 0.1)
            {
                double y = slope * x + intercept;
                lineSeries.Points.Add(new DataPoint(x, y));
            }

            plotModel.Series.Add(lineSeries);
            plotModel.InvalidatePlot(true);
        }

        /// <summary>
        /// Plots a quadratic equation given its coefficients.
        /// </summary>
        private void PlotCurve(double a, double b, double c)
        {
            plotModel.Series.Clear();

            var curveSeries = new LineSeries
            {
                Title = $"y = {a}x² + {b}x + {c}",
                StrokeThickness = 2
            };

            for (double x = -10; x <= 10; x += 0.1)
            {
                double y = a * x * x + b * x + c;
                curveSeries.Points.Add(new DataPoint(x, y));
            }

            plotModel.Series.Add(curveSeries);
            plotModel.InvalidatePlot(true);
        }

        /// <summary>
        /// Parses an equation and determines whether it is linear or quadratic.
        /// </summary>
        private (bool isLinear, double slope, double intercept, double a, double b, double c) ParseEquation(string equation)
        {
            equation = equation.Replace(" ", "");

            if (equation.StartsWith("y="))
            {
                equation = equation.Substring(2);
            }

            if (equation.Contains("x²") || equation.Contains("x^2"))
            {
                var match = System.Text.RegularExpressions.Regex.Match(equation, @"(?<a>[-+]?\d*\.?\d*)x\^2(?<b>[-+]?\d*\.?\d*)x(?<c>[-+]?\d+\.?\d*)");
                if (match.Success)
                {
                    double a = double.TryParse(match.Groups["a"].Value, out a) ? a : 1;
                    double b = double.TryParse(match.Groups["b"].Value, out b) ? b : 0;
                    double c = double.TryParse(match.Groups["c"].Value, out c) ? c : 0;
                    return (false, 0, 0, a, b, c);
                }
            }
            else if (equation.Contains("x"))
            {
                var match = System.Text.RegularExpressions.Regex.Match(equation, @"(?<m>[-+]?\d*\.?\d*)x(?<c>[-+]?\d*\.?\d*)");
                if (match.Success)
                {
                    double slope = double.TryParse(match.Groups["m"].Value, out slope) ? slope : 1;
                    double intercept = double.TryParse(match.Groups["c"].Value, out intercept) ? intercept : 0;
                    return (true, slope, intercept, 0, 0, 0);
                }
            }

            throw new FormatException("Invalid equation format. Use 'y=ax+b' or 'y=ax²+bx+c'.");
        }
    }
}
