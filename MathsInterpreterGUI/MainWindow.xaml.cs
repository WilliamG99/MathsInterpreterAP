using System;
using System.Windows;
using System.Windows.Controls;
using OxyPlot;
using MathsInterpreterBackend;

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
            InitializePlot(); // Initialize the plot area on startup
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

        private void InsertSymbol_Click(object sender, RoutedEventArgs e)
        {
            var button = sender as Button;
            if (button != null)
            {
                ExpressionTextBox.Text += button.Content.ToString();
            }
        }

        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            ExpressionTextBox.Clear();
            ResultTextBlock.Text = string.Empty;
            ErrorTextBlock.Text = string.Empty;
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            string buttonText = (sender as Button).Content.ToString();
            ExpressionTextBox.Text += buttonText;
        }

        private void CalculateButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                var result = MathsInterpreterBackend.Parser.interpret(ExpressionTextBox.Text);
                ResultTextBlock.Text = "= " + result.ToString();
                ErrorTextBlock.Text = string.Empty;
                UpdateVariableList();
            }
            catch (Exception ex)
            {
                ErrorTextBlock.Text = $"Error: {ex.Message}";
                ResultTextBlock.Text = string.Empty;
            }
        }

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

        private void InitializePlot()
        {
            plotModel = MathsInterpreterBackend.Plotting.createPlotModel("Graph");
            PlotView.Model = plotModel;
        }

        private void OnPlotLineClick(object sender, RoutedEventArgs e)
        {
            try
            {
                string equation = GraphEquationInput.Text;

                // Early validation before attempting parsing
                if (string.IsNullOrWhiteSpace(equation))
                {
                    MessageBox.Show("Please enter a valid equation.", "Invalid Input", MessageBoxButton.OK, MessageBoxImage.Warning);
                    return;
                }

                var parsedEquation = MathsInterpreterBackend.Plotting.parseEquation(equation);

                // Optionally clear the plot before adding the new equation
                plotModel.Series.Clear();

                MathsInterpreterBackend.Plotting.plotEquation(parsedEquation, plotModel);
                plotModel.InvalidatePlot(true); // Refresh the plot
            }
            catch (FormatException ex)
            {
                MessageBox.Show($"Invalid equation format: {ex.Message}", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"An error occurred: {ex.Message}", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

    }
}
