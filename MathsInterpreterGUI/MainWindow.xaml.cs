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
                //if (ExpressionTextBox.Text.Contains("="))
                //{
                //    // Use the equation solver for inputs with '='
                //    var result = MathsInterpreterBackend.Solver.solveLinearEquation(ExpressionTextBox.Text);
                //    ResultTextBlock.Text = $"Result: {result}";
                //}
                //else
                {
                    // Regular expression evaluation
                    var result = MathsInterpreterBackend.Parser.interpret(ExpressionTextBox.Text);
                    ResultTextBlock.Text = "= " + result.ToString();
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
