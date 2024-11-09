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
                int result = MathsInterpreterBackend.Parser.interpret(ExpressionTextBox.Text);

                ResultTextBlock.Text = $"Result: {result}";
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
