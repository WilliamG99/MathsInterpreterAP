using System;
using System.Windows;
using System.Windows.Controls;
using MathsInterpreterBackend;
using Microsoft.FSharp.Collections;

namespace MathsInterpreterGUI
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void CalculateButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                string input = ExpressionTextBox.Text;

                if (input.Contains("="))
                {
                    // Use the equation solver for inputs with '='
                    string result = MathsInterpreterBackend.Solver.solveLinearEquation(input);
                    ResultTextBlock.Text = $"Result: {result:F2}";
                }
                else
                {
                    // Regular expression evaluation
                    double result = MathsInterpreterBackend.Parser.interpret(input);
                    ResultTextBlock.Text = $"Result: {result:F2}";  // Display with 2 decimal places
                }

                ErrorTextBlock.Text = ""; // Clear error message if calculation is successful
            }
            catch (Exception ex)
            {
                ErrorTextBlock.Text = $"Error: {ex.Message}";
                ResultTextBlock.Text = "";
            }
        }




        private void ExpressionTextBox_TextChanged(object sender, TextChangedEventArgs e)
        {
            PlaceholderText.Visibility = string.IsNullOrEmpty(ExpressionTextBox.Text) ? Visibility.Visible : Visibility.Hidden;
        }
    }
}
