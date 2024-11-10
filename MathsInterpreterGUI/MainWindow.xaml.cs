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
                // Store the result as double to capture decimal precision from F#
                double result = MathsInterpreterBackend.Parser.interpret(ExpressionTextBox.Text);

                // Format the result to display with decimal places
                ResultTextBlock.Text = $"Result: {result:F2}";  // Display with 2 decimal places
                ErrorTextBlock.Text = "";
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
